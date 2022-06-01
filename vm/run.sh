#!/bin/bash

# Initial heap size for the Boehm GC.
export GC_INITIAL_HEAP_SIZE=10M
#export GC_PRINT_STATS=true
#export GC_DUMP_REGULARLY=true

#nim compile --run -d:debug --gc:boehm --verbosity:0 --outdir:target -p:src src/${1:-vm.nim} ${@:2}
nim compile --run -d:danger --passC:-flto --passL:-flto --gc:boehm --verbosity:0 --outdir:target -p:src src/${1:-vm.nim} ${@:2}
