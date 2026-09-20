#!/bin/bash
# Script to run tests with coverage and generate reports

set -e

COV_DIR="coverage"

# Find the correct gcov (matching gfortran version)
GCOV=$(dirname $(which gfortran))/gcov

echo "Using gcov: $GCOV"
echo ""
echo "================================================"
echo "Running tests with coverage..."
echo "================================================"

# Build and run tests with coverage flags
fpm test --flag "--coverage"
fpm run --example --flag "--coverage"

echo ""
echo "================================================"
echo "Generating coverage report with lcov..."
echo "================================================"

# Create coverage directory and move build artifacts
mkdir -p "$COV_DIR"
cp -r build/gfortran_*/*/* "$COV_DIR/" 2>/dev/null || true
# we want to skip the files that start with 'build_dependencies' because they are not part of the source code
find "$COV_DIR" -name "build_dependencies*" -exec rm -rf {} +
find "$COV_DIR" -name "test_*" -exec rm -rf {} +

# Capture initial coverage (baseline)
lcov --ignore-errors inconsistent --gcov-tool "$GCOV" --capture --initial --base-directory . --directory "$COV_DIR" --output-file "$COV_DIR/coverage.base" --quiet

# Capture test execution coverage
lcov --ignore-errors inconsistent --gcov-tool "$GCOV" --capture --base-directory . --directory "$COV_DIR" --output-file "$COV_DIR/coverage.capture" --quiet

# Combine baseline and test coverage
lcov --add-tracefile "$COV_DIR/coverage.base" --add-tracefile "$COV_DIR/coverage.capture" --output-file "$COV_DIR/coverage.info" --quiet

echo ""
echo "================================================"
echo "Coverage Summary"
echo "================================================"

# Display summary
lcov --summary "$COV_DIR/coverage.info"

echo ""
echo "Coverage data saved to: $COV_DIR/coverage.info"
echo ""
echo "generate HTML report:"

genhtml $COV_DIR/coverage.info --output-directory $COV_DIR/html

echo ""
