#!/bin/bash
c=`find ~/work/lore/compiler -type f \( -name "*.nim" -or -name "*.ts" -or -name "*.lore" -or -name "*.scala" \) -exec cat {} + | wc -w`
p=`find ~/work/lore/pyramid -type f \( -name "*.nim" -or -name "*.ts" -or -name "*.lore" -or -name "*.scala" \) -exec cat {} + | wc -w`
r=`find ~/work/lore/runtime -type f \( -name "*.nim" -or -name "*.ts" -or -name "*.lore" -or -name "*.scala" \) -exec cat {} + | wc -w`
t=`find ~/work/lore/test -type f \( -name "*.nim" -or -name "*.ts" -or -name "*.lore" -or -name "*.scala" \) -exec cat {} + | wc -w`
v=`find ~/work/lore/vm -type f \( -name "*.nim" -or -name "*.ts" -or -name "*.lore" -or -name "*.scala" \) -exec cat {} + | wc -w`

echo $((c + p + r + t + v))
