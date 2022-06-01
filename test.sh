#!/bin/bash
sbt "compiler/run compile test"

printf "\n"
printf '%*s\n' "${COLUMNS:-$(tput cols)}" '' | tr ' ' =
printf "\n"

cd vm
./run.sh vm.nim test ../lore_target/binary.poem
