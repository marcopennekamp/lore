#!/bin/bash
if ! sbt "compiler/run compile test"; then
  exit 1
fi

printf "\n"
printf '%*s\n' "${COLUMNS:-$(tput cols)}" '' | tr ' ' =
printf "\n"

cd vm
./run.sh vm.nim test ../lore_target/binary.poem
