# OCaml Bitarrays

![Build Status](https://github.com/adaminsky/Bitarray/workflows/BitarrayExt%20build/badge.svg)

**01001111 01000011 01100001 01101101 01101100**

A fixed size bitarray type implemented in OCaml using unboxed 63 bit integers.
This should technically work on both 64bit and 32bit architectures, but on a
32bit architecture it just uses a wrapped 64bit integer so that it respects the
same semantics of the 63bit integers. Therefore, it is likely that all
performance benefits will be lost if using a 32bit machine.

## Building and Testing

Run `dune build`. The library can be tested with `utop` by executing `dune utop
src/`. Running `dune runtest` runs the unit tests in the `test/` directory.

## Install

Run `dune install`.

## Rationale

- OCaml does not come packaged with support for arbitrary sized bitarrays. There
  already exists a couple libraries that provide this functionality, but they do
  not implement any arithmetic operations and their logical operations seem
  limited.
- Having arithmetic operations part of the bitarray is nice since then this
  essentially provides the functionality of any sized integers with an extremely
  compact representation.
- Implementing low-level contructs in a high-level language is fun.
