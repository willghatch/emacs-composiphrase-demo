#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

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
echo "  Repo: $REPO_DIR"
echo "  Test files: ${TEST_FILES[*]}"
emacs -batch \
    -L "$REPO_DIR" \
    -l ert \
    "${LOAD_ARGS[@]}" \
    -f ert-run-tests-batch-and-exit
