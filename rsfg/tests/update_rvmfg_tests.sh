#!/bin/sh

# The few rvmfg tests there are depend on bcfg files. This compiles the
# complete rsfg test suite for its use.

for f in tests/scripts/*.sfg; do
    cargo run -- $f ../rvmfg/tests
done
