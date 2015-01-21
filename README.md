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


## Extended Types

MSGPACK allows for a range of "Extended Types"; these consist of one of the
bytes `#xC7` to `#xC9` resp. `#xD4` to `#xD8`, a one-byte _type number_, and an
array of bytes for the data (which can optionally be interpreted as an integer ID).

A simple use case is eg. to identify pieces of data across a messagepack-RPC
channel (like eg. http://github.com/neovim/neovim does):

    (defparameter *my-type-list*
      (messagepack:define-extension-types
        '(:numeric
          0
          Buffer
          Window
          Tabpage
          ...)))

    (defparameter *my-lookup-table*
      (make-array 10 :adjustable t :initial-element nil))

    (let ((messagepack:*extended-types* *my-type-list*))
          (messagepack:*lookup-table* *my-lookup-table*)
      (messagepack:decode-stream stream))

Now receiving an reply with an item of extended type 0 will
automatically build an instance of the class `BUFFER`, and the `ID` slot will
be filled with the received ID, so that passing that instance to another
query can be converted into a matching messagepack _extended type_ element.

This provides type-safe communication across this RPC link.

The classes don't have to be defined ahead of time; the call to
DEFINE-EXTENSION-TYPES will create their definition if needed.

Please remember that only the _id_ gets transmitted; if you want to get the
_same_ object (with _same_ as in `EQ`), you'll need to make sure that
the correct object is looked up again; this is what `*LOOKUP-TABLE*`
above is for. Remember to bind that per RPC-connection to avoid duplicate IDs,
and to invalidate it if the remote process changes!


For more advanced usage `CL-MESSAGEPACK` provides a base class `EXTENSION-TYPE`
that can be used to define classes with more slots:

    (defclass type1 (cl-messagepack:extension-type)
       ( slots... ))


Please note that *encoding* is currently limited to the `#xC7` byte, and
therefore imposes a 255 byte limit for the byte array.


## Testing

Copy the `cl-messagepack` directory to the `local-projects` directory
of your Quicklisp install, then

    (require :cl-messagepack)
    (fiveam:run! 'mpk-tests::cl-messagepack-tests)

in a REPL (tested under SBCL and CCL under Linux x64).
