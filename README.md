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

### Earlier implementation of CL-MESSAGEPACK

The previous version used the bytes `#xC4`, `#xC5`, `#xC6`, and `#xC7` to encode
a 'pure' cons (a cons whose `CDR` is not a cons,
symbols, symbols as a number (via a lookup table), and rationals.

As these encodings are no longer allowed by the MSGPACK specification this
functionality has been removed; you can use `*EXTENDED-TYPES*`
to achieve similar things, though.


## Testing

Copy the `cl-messagepack` directory to the `local-projects` directory
of your Quicklisp install, then 

    (require :cl-messagepack)
    (fiveam:run! 'mpk-tests::cl-messagepack-tests)

in a REPL (tested under SBCL and CCL under Linux x64).
