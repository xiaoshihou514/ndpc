#! /usr/bin/env bash
cd typst
for f in *.typ; do
  if typst compile "$f"; then
    echo "✓ $f typst ok"
  else
    echo "✗ $f typst bad"
    exit 1
  fi
done
