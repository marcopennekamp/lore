type
  ## An ImSeq is an immutable sequence. It has shallow copying semantics and should thus be preferred over Nim's `seq`
  ## when the elements are known exactly at sequence construction. Its `elements` are embedded in the object, so that
  ## there is only need for a single allocation.
  ImSeq*[T] = ref ImSeqObj[T]

  ImSeqObj[T] = object
    len*: int
    elements*: UncheckedArray[T]

proc new_fixed_seq*[T](source: open_array[T]): ImSeq[T] =
  let length = source.len
  # Note that `sizeof` of an unchecked array is 0, so we can use `sizeof(ImSeqObj[T])` to get the size of the preamble.
  var seq = cast[ImSeq[T]](alloc(sizeof(ImSeqObj[T]) + length * sizeof(T)))
  seq.len = length
  copy_mem(addr seq.elements, unsafe_addr source, length * sizeof(T))
  seq

let empty = new_fixed_seq[uint64]([])

## Returns an empty immutable sequence for the given element type. This is backed by a single object reference so that
## reallocations do not have to occur.
proc empty_fixed_seq*[T](): ImSeq[T] = cast[ImSeq[T]](empty)

proc `[]`*[T](seq: ImSeq[T], index: int): T = seq.elements[index]

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
  var i = 1
  while i < length:
    str.add(separator)
    str.add($seq[i])
    i += 1
  str

proc `$`*[T](seq: ImSeq[T]): string =
  "fseq[" & seq.join(", ") & "]"
