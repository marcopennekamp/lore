#!/bin/bash
nim compile --run -d:danger --verbosity:0 --outdir:target -p:src src/${1:-vm.nim}
