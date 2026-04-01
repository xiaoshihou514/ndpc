#! /usr/bin/env bash
source $HOME/.elan/env
cd lean
for f in *.lean; do
  if lean "$f"; then
    echo "✓ $f lean ok"
  else
    echo "✗ $f lean bad"
    exit 1
  fi
done
