# Executes the given code if the build is in release mode.
macro when_release*(code: untyped): untyped =
  when defined(release) or defined(danger):
    code
  else:
    discard

# Executes the given code if the build is not in release mode.
macro when_debug*(code: untyped): untyped =
  when not defined(release) or not defined(danger):
    code
  else:
    discard
