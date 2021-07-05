#!/bin/bash

# Build the language server JAR and copy it to the client's target directory.
cd ..
sbt assembly
cp lsp/server/target/scala-2.13/lore-language-server-assembly-0.1.0-SNAPSHOT.jar lsp/client/out/lore-language-server.jar
