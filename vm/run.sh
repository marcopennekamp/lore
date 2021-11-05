#!/bin/bash

# Initial heap size for the Boehm GC.
export GC_INITIAL_HEAP_SIZE=10M
#export GC_PRINT_STATS=true

# --boundChecks:off --overflowChecks:off
nim compile --run -d:danger --passC:-flto --passL:-flto --gc:boehm --verbosity:0 --outdir:target -p:src src/${1:-vm.nim} ${@:2}
