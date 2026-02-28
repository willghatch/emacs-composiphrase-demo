#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
DEPS_DIR="$SCRIPT_DIR/dependencies"
CARETTEST_DIR="$DEPS_DIR/carettest"
ESTATE_DIR="$DEPS_DIR/estate"

mkdir -p "$DEPS_DIR"

if [ ! -d "$CARETTEST_DIR" ]; then
    echo "Cloning carettest dependency..."
    git clone https://github.com/willghatch/emacs-carettest "$CARETTEST_DIR"
fi

if [ ! -d "$ESTATE_DIR" ]; then
    echo "Cloning estate dependency..."
    git clone https://github.com/willghatch/emacs-estate "$ESTATE_DIR"
fi

# Collect test files
TEST_FILES=()
for f in "$SCRIPT_DIR"/test-*.el; do
    [ -f "$f" ] && TEST_FILES+=("$f")
done

if [ ${#TEST_FILES[@]} -eq 0 ]; then
    echo "No test files found matching test-*.el"
    exit 1
fi

LOAD_ARGS=()
for f in "${TEST_FILES[@]}"; do
    LOAD_ARGS+=(-l "$f")
done

echo "Running tests..."
echo "  Estate: $ESTATE_DIR"
echo "  Repo:   $REPO_DIR"
echo "  Test files: ${TEST_FILES[*]}"
emacs -batch \
    -L "$ESTATE_DIR" \
    -L "$REPO_DIR" \
    -L "$CARETTEST_DIR" \
    -l ert \
    "${LOAD_ARGS[@]}" \
    -f ert-run-tests-batch-and-exit
