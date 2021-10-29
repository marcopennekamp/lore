from bytecode import Function
from values import Value

type
  # For initial development, we're building the test programs in the correct data structure right away.
  Example* = ref object
    name*: string
    function*: Function
    arguments*: seq[Value]
    runs*: int
