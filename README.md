-*- markdown -*-

# `CL-MESSAGEPACK`

A Common Lisp implementation of the MessagePack (http://msgpack.org/)
serialization/deserialization format, implemented according to
http://wiki.msgpack.org/display/MSGPACK/Format+specification.

Depends on `flexi-streams` and `babel`. Floating point values are
supported only on SBCL currently.

Status: first draft encoder and decoder implemented, added extensions
for some Lisp data types (see below), simple tests.

## Extensions to the Message Pack specification

### (C0) 'NIL'

This translates to `NIL` in Lisp directly, but see `C2` (`False`) below, too.

### (C2) 'False'

On encoding this can be achieved via `:false`; when this is encountered 
during decoding, `NIL` is returned to Lisp as long `*use-false*` is kept `NIL`.

### (C4) 'Pure' Cons

A 'pure' cons is a cons whose `CDR` is not a cons. Regular lists are
encoded as Message Pack arrays, but we need something special for
conses.

The pure cons is encoded as byte #xC4, followed by the encodings of
the `CAR` of the cons and the encoding of the `CDR` of the cons.

### (C5) Symbol

In order to be able to restore the symbols as symbols, we can't simply
encode them as strings. So a symbol will be encoded as byte #xC5,
followed by the name of the package of the symbol, followed by the
name of the symbol.

### (C6) Symbol as number

Since encoding symbols should be shorter than encoding strings, we can
encode them as numbers, provided that the encoder and decoder both
have knowledge of a symbol <-> integer bijection between the symbols
and the numbers. A symbol that is found in this table will be encoded
as byte #xC6, followed by the encoding of its corresponding integer.

### (C7) Rationals

Rationals are encoded as byte #xC7, followed by the encoding of the
numerator, and the encoding of the denominator. If the numerator
cannot be encoded as an integer (it is too large to be encoded as an
uint64 or an int64, it will be encoded as a string. The same happens
for large denominators).

## Testing

Copy the `cl-messagepack` directory to the `local-projects` directory
of your Quicklisp install, then 

    (require :cl-messagepack)
    (fiveam:run! 'mpk-tests::cl-messagepack-tests)

in a REPL (tested under SBCL and CCL under Linux x64).
