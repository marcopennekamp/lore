#!/bin/bash

# If you pass a folder name as the first parameter, Deno will only execute tests contained in the folder. For example,
# running `./test.sh dispatch` will only execute dispatch tests.

echo "Running Lore functional tests..."

# Remove the old lore.jar.
rm test/lore.jar

# Compile and package the JAR.
sbt --error assembly

# Copy the compiler JAR to the test folder.
cp compiler/target/scala-2.13/lore-assembly-0.1.0-SNAPSHOT.jar test/lore.jar

printf "\n"

# Run all functional tests.
cd test
deno test $1 --allow-run
