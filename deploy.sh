#!/usr/bin/env bash
# Deploy rendered dashboard to GitHub Pages (gh-pages branch)
# Usage: bash deploy.sh
#
# Prerequisites: KOBO_TOKEN must be set (via .Renviron or env var)
# What it does: renders the Quarto dashboard, copies output to gh-pages, pushes

set -e

REPO_ROOT="$(cd "$(dirname "$0")" && pwd)"
SITE_DIR="$REPO_ROOT/dashboard/_site"
TMP_DIR=$(mktemp -d)

# 1. Render
echo ">> Rendering dashboard..."
cd "$REPO_ROOT/dashboard"
quarto render index.qmd
quarto render trends.qmd

# 2. Copy rendered output to temp (survives branch switch)
echo ">> Copying site to temp dir..."
cp -r "$SITE_DIR"/* "$TMP_DIR/"

# 3. Switch to gh-pages orphan branch
echo ">> Switching to gh-pages..."
cd "$REPO_ROOT"
git stash --quiet 2>/dev/null || true
git checkout gh-pages 2>/dev/null || git checkout --orphan gh-pages

# 4. Clear tracked files, copy fresh output
git rm -rf . --quiet 2>/dev/null || true
cp -r "$TMP_DIR"/* .
rm -rf "$TMP_DIR"

# 5. Stage only site files, commit, push
git add -A
git commit -m "Deploy $(date +%Y-%m-%d)"
git push upstream gh-pages

# 6. Return to main
git checkout main --quiet
git stash pop --quiet 2>/dev/null || true

echo ">> Done! Site will update at https://mattykuch.github.io/jc-farms/ in ~1 minute."
