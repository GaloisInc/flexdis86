#!/usr/bin/env python3
"""Compare flexdis86 build/test metrics between two git refs.

Usage:
    python3 scripts/compare.py [ref1 [ref2]]

Defaults to HEAD~1 vs HEAD.  Each stats comparison should be
between the previous commit (HEAD~1) and the current one (HEAD).

Output is a pair of Markdown tables:
  1. Build & artifact sizes (build time, peak GHC heap RSS, .a/.so/test-binary sizes)
  2. Test suite GHC RTS stats (heap allocated, max live, GC CPU time, total CPU time)

All metrics are collected at -O0, -O1, and -O2.

Requirements:
  - cabal and ghc on PATH
  - GNU time or /usr/bin/time (optional, used only as fallback)
  - The test suite binary must be built with -rtsopts (or the default
    -rtsopts=some) for test-suite stats to be collected.
"""

import argparse
import os
import re
import subprocess
import sys
import tempfile
import time
from pathlib import Path

OPT_LEVELS = [0, 1, 2]


# ── subprocess helpers ────────────────────────────────────────────────────────

def run(cmd, cwd=None, capture=True, timeout=900, check=False):
    """Run a shell command, optionally capturing merged stdout+stderr."""
    kw = dict(shell=True, cwd=cwd, text=True, timeout=timeout)
    if capture:
        kw.update(stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    r = subprocess.run(cmd, **kw)
    if check and r.returncode != 0:
        raise subprocess.CalledProcessError(
            r.returncode, cmd, getattr(r, 'stdout', None))
    return r


def git(cmd, cwd):
    return run(f"git {cmd}", cwd=cwd, check=True)


# ── git worktree helpers ──────────────────────────────────────────────────────

def add_worktree(repo, path, ref):
    git(f"worktree add --detach {path} {ref}", repo)


def remove_worktree(repo, path):
    run(f"git worktree remove --force {path}", cwd=repo)


# ── GHC RTS stats parsers ─────────────────────────────────────────────────────

def parse_build_rts(text):
    """From the merged output of a 'cabal build' that was run with
    --ghc-options='+RTS -s -RTS', return the peak Maximum residency
    (in bytes) across all GHC module-compilation invocations."""
    max_res = 0
    # GHC +RTS -s output line:
    #   123,456 bytes maximum residency (N sample(s))
    for m in re.finditer(r'([\d,]+)\s+bytes maximum residency', text):
        val = int(m.group(1).replace(',', ''))
        max_res = max(max_res, val)
    return max_res or None


def parse_test_rts(text):
    """Parse GHC +RTS -s output from a test-binary run.

    Returns a dict with keys:
        heap_allocated   – total bytes allocated in the heap
        max_live         – maximum residency (bytes)
        gc_cpu           – GC CPU time (seconds)
        total_cpu        – total CPU time (seconds)
    Missing fields are None.
    """
    def find(pat):
        m = re.search(pat, text, re.MULTILINE)
        return m.group(1) if m else None

    def inum(s):
        return int(s.replace(',', '')) if s else None

    def fnum(s):
        return float(s) if s else None

    return {
        'heap_allocated': inum(find(r'([\d,]+)\s+bytes allocated in the heap')),
        'max_live':       inum(find(r'([\d,]+)\s+bytes maximum residency')),
        'gc_cpu':         fnum(find(r'GC\s+time\s+([\d.]+)s')),
        'total_cpu':      fnum(find(r'Total\s+time\s+([\d.]+)s')),
    }


# ── artifact-size helpers ─────────────────────────────────────────────────────

def artifact_sizes(worktree):
    """Return a dict with keys static_a, shared_so, test_bin (bytes).
    Keys are absent when the corresponding file was not found."""
    d = Path(worktree) / 'dist-newstyle'
    if not d.exists():
        return {}
    sizes = {}

    # Static library: largest .a file whose name contains 'flexdis86'
    # (but not the test binary helper libs).
    a_files = [
        f for f in d.rglob('*.a')
        if 'flexdis86' in f.name
        and 'test' not in f.stem.lower()
        and f.stat().st_size > 0
    ]
    if a_files:
        sizes['static_a'] = max(f.stat().st_size for f in a_files)

    # Shared library (.so on Linux, .dylib on macOS)
    so_files = [
        f for f in list(d.rglob('*.so')) + list(d.rglob('*.dylib'))
        if 'flexdis86' in f.name and f.stat().st_size > 0
    ]
    if so_files:
        sizes['shared_so'] = max(f.stat().st_size for f in so_files)

    # Test binary
    tb = [
        f for f in d.rglob('flexdis86-tests')
        if f.is_file() and os.access(f, os.X_OK)
    ]
    if tb:
        sizes['test_bin'] = max(f.stat().st_size for f in tb)

    return sizes


# ── per-ref / per-opt measurement ─────────────────────────────────────────────

def _write_project_local(worktree, opt):
    """Append opt-level + GHC RTS stat flags for flexdis86 to
    cabal.project.local, returning the original content for later restore."""
    p = Path(worktree) / 'cabal.project.local'
    original = p.read_text() if p.exists() else ''
    addition = (
        f'\n-- injected by compare.py\n'
        f'package flexdis86\n'
        f'  ghc-options: -O{opt} +RTS -s -RTS\n'
    )
    p.write_text(original + addition)
    return original


def _restore_project_local(worktree, original):
    p = Path(worktree) / 'cabal.project.local'
    if original:
        p.write_text(original)
    else:
        p.unlink(missing_ok=True)


def measure_build(worktree, opt):
    """Build the project at the given opt level.

    Returns a dict with keys:
        build_time   – wall-clock seconds for 'cabal build all'
        peak_rss     – peak Maximum residency across all GHC invocations (bytes)
        static_a     – static library size (bytes), if found
        shared_so    – shared library size (bytes), if found
        test_bin     – test binary size (bytes), if found
    Returns None on build failure.
    """
    run('cabal clean', cwd=worktree)
    original = _write_project_local(worktree, opt)
    try:
        t0 = time.monotonic()
        r = run('cabal build all', cwd=worktree, timeout=900)
        elapsed = time.monotonic() - t0
    finally:
        _restore_project_local(worktree, original)

    if r.returncode != 0:
        print(f'  build -O{opt} FAILED:\n{r.stdout[-3000:]}', file=sys.stderr)
        return None

    m = {'build_time': elapsed}
    rss = parse_build_rts(r.stdout)
    if rss is not None:
        m['peak_rss'] = rss
    m.update(artifact_sizes(worktree))
    return m


def measure_test(worktree, opt):
    """Run the test suite with +RTS -s and return parsed stats (dict).

    The test binary must be compiled with -rtsopts (or the default
    -rtsopts=some, which allows -s) for RTS stats to be collected.
    An empty dict is returned on failure or if stats are unavailable.
    """
    r = run(
        'cabal test flexdis86-tests --test-options="+RTS -s -RTS"',
        cwd=worktree, timeout=600
    )
    if r.returncode != 0:
        print(f'  test -O{opt} FAILED', file=sys.stderr)
        return {}
    return parse_test_rts(r.stdout)


# ── formatting helpers ────────────────────────────────────────────────────────

def fmt_mb(n):
    if n is None:
        return 'N/A'
    if n >= 1_000_000_000:
        return f'{n / 1e9:.2f} GB'
    return f'{n / 1e6:.1f} MB'


def fmt_sec(s):
    return f'{s:.3f}s' if s is not None else 'N/A'


def fmt_change(a, b):
    if a is None or b is None or a == 0:
        return 'N/A'
    pct = (b - a) / a * 100
    arrow = '▲' if pct >= 0 else '▼'
    return f'{arrow}{pct:+.1f}%'


def md_table(caption, rows):
    """Render a Markdown table.  rows[0] is the header row."""
    widths = [
        max(len(str(rows[r][c])) for r in range(len(rows)))
        for c in range(len(rows[0]))
    ]

    def fmt_row(cells):
        return '| ' + ' | '.join(
            str(c).ljust(widths[i]) for i, c in enumerate(cells)
        ) + ' |'

    sep = '|' + '|'.join('-' + '-' * w + '-' for w in widths) + '|'
    lines = [f'## {caption}', '', fmt_row(rows[0]), sep]
    for row in rows[1:]:
        lines.append(fmt_row(row))
    return '\n'.join(lines)


# ── main ──────────────────────────────────────────────────────────────────────

def main():
    ap = argparse.ArgumentParser(
        description='Compare flexdis86 build/test metrics between two git refs.',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog='Defaults to HEAD~1 vs HEAD.',
    )
    ap.add_argument('ref1', nargs='?', default='HEAD~1',
                    help='baseline ref (default: HEAD~1)')
    ap.add_argument('ref2', nargs='?', default='HEAD',
                    help='comparison ref (default: HEAD)')
    ap.add_argument('--repo', default='.',
                    help='repository root (default: current directory)')
    args = ap.parse_args()

    repo = os.path.abspath(args.repo)
    r1, r2 = args.ref1, args.ref2

    def resolve(ref):
        return run(f'git rev-parse --short {ref}', cwd=repo,
                   check=True).stdout.strip()

    h1, h2 = resolve(r1), resolve(r2)
    lbl1 = f'`{r1}` ({h1})'
    lbl2 = f'`{r2}` ({h2})'
    print(f'Comparing {lbl1}  vs  {lbl2}', file=sys.stderr)

    data = {r1: {}, r2: {}}

    with tempfile.TemporaryDirectory(prefix='flexdis86-cmp-') as tmp:
        wt = {}
        try:
            for ref in (r1, r2):
                path = os.path.join(
                    tmp,
                    ref.replace('/', '_').replace('~', '_').replace('^', '_')
                )
                print(f'\nCreating worktree for {ref} ...', file=sys.stderr)
                add_worktree(repo, path, ref)
                wt[ref] = path

            for ref, path in wt.items():
                for opt in OPT_LEVELS:
                    print(f'\n=== {ref}  -O{opt} ===', file=sys.stderr)

                    print(f'  building...', file=sys.stderr)
                    bm = measure_build(path, opt)

                    print(f'  testing...', file=sys.stderr)
                    tm = measure_test(path, opt)

                    if bm is not None:
                        data[ref][opt] = {
                            **bm,
                            **{'t_' + k: v for k, v in tm.items()},
                        }

        finally:
            for ref, path in wt.items():
                try:
                    remove_worktree(repo, path)
                except Exception as e:
                    print(f'Warning: failed to remove worktree {path}: {e}',
                          file=sys.stderr)

    hdr = ['Metric', 'Opt', lbl1, lbl2, 'Change']

    # ── Build & artifact-sizes table ──────────────────────────────────────
    build_rows = [hdr]
    build_metrics = [
        ('Build time',        'build_time', fmt_sec),
        ('Peak GHC heap RSS', 'peak_rss',   fmt_mb),
        ('Static lib (.a)',   'static_a',   fmt_mb),
        ('Shared lib (.so)',  'shared_so',  fmt_mb),
        ('Test binary',       'test_bin',   fmt_mb),
    ]
    for label, key, fmt in build_metrics:
        for opt in OPT_LEVELS:
            v1 = data[r1].get(opt, {}).get(key)
            v2 = data[r2].get(opt, {}).get(key)
            build_rows.append(
                [label, f'`-O{opt}`', fmt(v1), fmt(v2), fmt_change(v1, v2)]
            )

    # ── Test-suite RTS-stats table ────────────────────────────────────────
    test_rows = [hdr]
    test_metrics = [
        ('Heap allocated', 't_heap_allocated', fmt_mb),
        ('Max live bytes', 't_max_live',        fmt_mb),
        ('GC CPU time',    't_gc_cpu',          fmt_sec),
        ('Total CPU time', 't_total_cpu',       fmt_sec),
    ]
    for label, key, fmt in test_metrics:
        for opt in OPT_LEVELS:
            v1 = data[r1].get(opt, {}).get(key)
            v2 = data[r2].get(opt, {}).get(key)
            test_rows.append(
                [label, f'`-O{opt}`', fmt(v1), fmt(v2), fmt_change(v1, v2)]
            )

    print(md_table('Build & artifact sizes', build_rows))
    print()
    print(md_table('Test suite (GHC RTS stats, deterministic)', test_rows))


if __name__ == '__main__':
    main()
