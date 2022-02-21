import std/hashes

type
  ImSeq*[T] = ref ImSeqObj[T]
    ## An ImSeq is an immutable sequence. It has shallow copying semantics and should thus be preferred over Nim's
    ## `seq` when the elements are known exactly at sequence construction. Its `elements` are embedded in the object,
    ## so that there is only need for a single allocation.
    ##
    ## Mutation of an immutable sequence is possible and ONLY allowed for *fresh* ImSeqs which have just been created
    ## or copied. An immutable sequence which has already left its creator's scope, such as an object property, must
    ## never be mutated.

  ImSeqObj[T] = object
    len*: int
    elements*: UncheckedArray[T]

  FixedSeq*[T] = ImSeq[T]
    ## A FixedSeq is an ImSeq with no immutability guarantees. By contract, a FixedSeq may be mutated at any time and
    ## its owner should treat it as such.

proc alloc_immutable_seq[T](length: int): ImSeq[T] =
  # Note that `sizeof` of an unchecked array is 0, so we can use `sizeof(ImSeqObj[T])` to get the size of the preamble.
  let seq = cast[ImSeq[T]](alloc0(sizeof(ImSeqObj[T]) + length * sizeof(T)))
  seq.len = length
  seq

let empty = alloc_immutable_seq[uint64](0)

proc empty_immutable_seq*[T](): ImSeq[T] {.inline.} = cast[ImSeq[T]](empty)
  ## Returns an empty immutable sequence for the given element type. This is backed by a single constant ImSeq so that
  ## this function makes no allocations.

proc new_immutable_seq*[T](length: int): ImSeq[T] =
  ## Creates a new immutable sequence of the given length with zeroed entries. This can be used to efficiently
  ## initialize an immutable sequence. If `length` is 0, the empty sequence is returned.
  if length == 0: empty_immutable_seq[T]()
  else: alloc_immutable_seq[T](length)

proc new_immutable_seq*[T](length: uint): ImSeq[T] = new_immutable_seq[T](int(length))

proc new_immutable_seq[T](source: pointer, length: int): ImSeq[T] =
  var seq = new_immutable_seq[T](length)
  copy_mem(addr seq.elements, source, length * sizeof(T))
  seq

proc new_immutable_seq*[T](source: ptr UncheckedArray[T], length: int): ImSeq[T] = new_immutable_seq[T](cast[pointer](source), length)

proc new_immutable_seq*[T](source: open_array[T]): ImSeq[T] = new_immutable_seq[T](unsafe_addr source, source.len)

proc new_immutable_seq*[T](source: open_array[T], length: int): ImSeq[T] =
  ## Creates a new immutable sequence from `source`, but taking only the first `length` elements.
  new_immutable_seq[T](unsafe_addr source, length)

proc new_immutable_seq*[T](source: ImSeq[T]): ImSeq[T] =
  ## Copies all elements of `source` into a new immutable sequence with the same length. A copied immutable sequence is
  ## guaranteed to be fresh and may thus be mutated for initialization.
  new_immutable_seq[T](addr source.elements, source.len)

proc `[]`*[T](seq: ImSeq[T], index: int): T = seq.elements[index]
proc `[]`*[T](seq: ImSeq[T], index: int64): T = seq.elements[index]
proc `[]`*[T](seq: ImSeq[T], index: uint): T = seq.elements[index]

proc `[]=`*[T](seq: var ImSeq[T], index: int, value: T) =
  ## Replaces the sequence's element at the given index. This is an unsafe operation, as ImSeqs should be immutable,
  ## but it may be used to optimize initialization.
  seq.elements[index] = value

proc `[]=`*[T](seq: var ImSeq[T], index: uint, value: T) =
  seq.elements[index] = value

template to_open_array*[T](seq: ImSeq[T]): open_array[T] =
  to_open_array(addr seq.elements, 0, seq.len - 1)

proc append*[T](old_seq: ImSeq[T], element: T): ImSeq[T] =
  let old_length = old_seq.len
  var new_seq = new_immutable_seq[T](old_length + 1)
  copy_mem(addr new_seq.elements, addr old_seq.elements, old_length * sizeof(T))
  new_seq[old_length] = element
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

proc `hash`*[T](seq: ImSeq[T]): Hash = hash(to_open_array(seq))

proc `$`*[T](seq: ImSeq[T]): string =
  "[" & seq.join(", ") & "]"
