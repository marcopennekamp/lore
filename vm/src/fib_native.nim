proc fib(n: int64): int64 =
  if n > 1:
    fib(n - 1) + fib(n - 2)
  else:
    n

proc test_fib*(): int64 = fib(10)
