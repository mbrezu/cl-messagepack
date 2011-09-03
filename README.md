-*- markdown -*-

# `CL-MESSAGEPACK`

A Common Lisp implementation of the MessagePack (http://msgpack.org/)
serialization/deserialization format, implemented according to
http://wiki.msgpack.org/display/MSGPACK/Format+specification.

Depends on `flexi-streams` and `babel`. Floating point values are
supported only on SBCL currently.

Status: first draft encoder and decoder implemented.

To do:

 * tests for encoding arrays etc should only test type-code and length
   encoding;
 * correct encoding of elements should be handled via decoding the
   encoded sequences and comparing with the original.

