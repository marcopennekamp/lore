#!/bin/bash

# Initial heap size for the Boehm GC.
export GC_INITIAL_HEAP_SIZE=10M
#export GC_PRINT_STATS=true

nim compile --run -d:danger --gc:boehm --verbosity:0 --outdir:target -p:src src/${1:-vm.nim}
