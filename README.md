-*- markdown -*-

# `CL-MESSAGEPACK`

A Common Lisp implementation of the MessagePack (http://msgpack.org/)
serialization/deserialization format, implemented according to
http://wiki.msgpack.org/display/MSGPACK/Format+specification.

Depends on `flexi-streams` and `babel`. Floating point values are
supported only on SBCL currently.

Status: first draft encoder and decoder implemented.

To do:

 * write tests;
 * write some benchmarks (compare speed against `cl-json`?)

