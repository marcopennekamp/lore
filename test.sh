#!/bin/bash

# TODO (assembly): Update this for the new VM runtime.

# If you pass a folder name as the first parameter, Deno will only execute tests contained in the folder. For example,
# running `./test.sh features/dispatch` will only execute dispatch tests.

# Compile and package the JAR.
sbt assembly > /dev/null

# If the `test/target/lore` executable doesn't exist or if the assembled jar's checksum is not the same as the checksum
# in `test/target/checksum`, compile the executable with native-image.
checksum=$(shasum compiler/target/scala-2.13/lore-assembly-0.1.0-SNAPSHOT.jar)
if [ ! -f "test/target/lore" ] || [ ! -f "test/target/checksum" ] || [[ $(head -1 test/target/checksum) != $checksum ]]; then
  echo "Compiling Lore to an executable with GraalVM native-image..."
  native-image --no-fallback -H:ReflectionConfigurationFiles=compiler/native-image/reflection.json -H:ResourceConfigurationFiles=compiler/native-image/resources.json -H:+AllowIncompleteClasspath -jar compiler/target/scala-2.13/lore-assembly-0.1.0-SNAPSHOT.jar test/target/lore
  echo "$checksum" > test/target/checksum
  printf "\n"
fi

printf "Running Lore functional tests...\n\n"

# Run all functional tests.
cd test
deno test $1 --allow-run
