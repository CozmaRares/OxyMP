#!/bin/sh

echo "Updating root crate..."
cargo update --color always

for lib in libs/*; do
  if [ -d "$lib" ]; then
    echo "Updating crate in $lib..."
    cd "$lib" 
    cargo update --color always
  fi
done

echo "All updates complete."
