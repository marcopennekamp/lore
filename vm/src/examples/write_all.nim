import "../poems"

import nine

## The purpose of this script is to write all Poem examples to `.poem` files so that they can be loaded by the VM.
when is_main_module:
  poems.write("target/nine.poem", nine.poem)
