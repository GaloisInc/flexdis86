#! /bin/bash
# This updates the dependencies to the latest versions, checking them out as needed.
set -e

cd "$(dirname "${BASH_SOURCE[0]}")/.."

if [ ! -f "flexdis86.cabal" ]; then
    >&2 echo "Please run this from the root flexdis directory."
    exit
fi

# Clone and update dependencies

PRIVATE_GITHUB_REPOS=(elf-edit)

mkdir -p deps
for dep in "${PRIVATE_GITHUB_REPOS[@]}"; do
  if ! [ -e deps/"$dep" ]; then
    echo "Cloning $dep"
    git clone git@github.com:GaloisInc/"$dep".git deps/"$dep"
  else
    echo "Pulling $dep"
    (cd deps/"$dep" && git pull)
  fi
done
