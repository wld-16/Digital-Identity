#!/usr/bin/env sh
set -e
npm run build
cd dist
git init
git add -A
git commit -m 'deploy'
git push -f https://github.com/spacewave1/spacewave1.github.io master:gh-pages
cd -