quiver-binary
=============

![Hackage](https://img.shields.io/hackage/v/quiver-binary.svg)](https://hackage.haskell.org/package/quiver-binary) [![Build Status](https://travis-ci.org/ivan-m/quiver-binary.svg)](https://travis-ci.org/ivan-m/quiver-binary)

This package provides inter-operability between the _[quiver]_
stream-processing library and the _[binary]_ serialisation library.

Using this you can efficiently encode/decode native Haskell values in
a streaming fashion.

This is especially useful when combined with the _[quiver-bytestring]_
library for I/O.

[quiver]: http://hackage.haskell.org/package/quiver
[binary]: https://hackage.haskell.org/package/binary
[quiver-bytestring]: http://hackage.haskell.org/package/quiver-bytestring
