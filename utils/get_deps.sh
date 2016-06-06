#!/bin/bash
#
# This script gets the latest version from the elf-edit repo needed to build flexdis86.
set -e

pkg="elf-edit"

if [ ! -f "elf-edit.cabal" ]; then
    echo >&2 "This script gets dependencies needed to build flexdis86."
    echo >&2 ""
    echo >&2 "This script should be run from the root directory of the repo."
    exit 1
fi

# Pull elf-edit
if [ ! -d "deps/$pkg" ]; then
   pushd deps > /dev/null
   git clone "git@github.com/GaloisInc/$pkg.git"
   popd > /dev/null
else
    pushd "deps/$pkg" > /dev/null
    git pull
    popd > /dev/null
fi
