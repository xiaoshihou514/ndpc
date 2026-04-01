#! /usr/bin/env bash
cd latex
for f in *.tex; do
  if pdflatex -interaction=nonstopmode "$f" > /dev/null; then
    echo "✓ $f latex ok"
  else
    echo "✗ $f latex bad"
    exit 1
  fi
done
