type
  ## An ImSeq is an immutable sequence. It has shallow copying semantics and should thus be preferred over Nim's `seq`
  ## when the elements are known exactly at sequence construction. Its `elements` are embedded in the object, so that
  ## there is only need for a single allocation.
  ImSeq*[T] = ref ImSeqObj[T]

  ImSeqObj[T] = object
    len*: int
    elements*: UncheckedArray[T]

## Creates a new immutable sequence of the given length with zeroed entries. This can be used to efficiently initialize
## an immutable sequence.
proc new_immutable_seq*[T](length: int): ImSeq[T] =
  # Note that `sizeof` of an unchecked array is 0, so we can use `sizeof(ImSeqObj[T])` to get the size of the preamble.
  var seq = cast[ImSeq[T]](alloc0(sizeof(ImSeqObj[T]) + length * sizeof(T)))
  seq.len = length
  seq

proc new_immutable_seq*[T](length: uint): ImSeq[T] = new_immutable_seq[T](int(length))

## Creates a new immutable sequence from the given source.
proc new_immutable_seq*[T](source: open_array[T]): ImSeq[T] =
  let length = source.len
  var seq = new_immutable_seq[T](length)
  copy_mem(addr seq.elements, unsafe_addr source, length * sizeof(T))
  seq

let empty = new_immutable_seq[uint64]([])

## Returns an empty immutable sequence for the given element type. This is backed by a single object reference so that
## reallocations do not have to occur.
proc empty_immutable_seq*[T](): ImSeq[T] = cast[ImSeq[T]](empty)

proc `[]`*[T](seq: ImSeq[T], index: int): T = seq.elements[index]
proc `[]`*[T](seq: ImSeq[T], index: int64): T = seq.elements[index]
proc `[]`*[T](seq: ImSeq[T], index: uint): T = seq.elements[index]

proc append*[T](old_seq: ImSeq[T], element: T): ImSeq[T] =
  let old_length = old_seq.len
  var new_seq = new_immutable_seq[T](old_length + 1)
  copy_mem(addr new_seq.elements, addr old_seq.elements, old_length * sizeof(T))
  new_seq.elements[old_length] = element
  new_seq

iterator items*[T](seq: ImSeq[T]): T {.inline.} =
  var i = 0
  let length = seq.len
  while i < length:
    yield seq[i]
    i += 1

proc join*[T](seq: ImSeq[T], separator: string): string =
  let length = seq.len
  if length == 0:
    return ""

  var str = $seq[0]
  for i in 1 ..< length:
    str.add(separator)
    str.add($seq[i])
  str

proc `$`*[T](seq: ImSeq[T]): string =
  "ImSeq[" & seq.join(", ") & "]"
