# b.el

[![](https://github.com/dragonwasrobot/b.el/actions/workflows/b-tests.yml/badge.svg)](https://github.com/dragonwasrobot/b.el/actions/workflows/b-tests.yml)

A utility library for parsing and manipulating bytes either as decimals,
hexadecimals or binaries. The main purposes of this library is for rapid
prototyping of binary protocol and learning about the representation and
manipulation of these fundamental building blocks.

The library operates on decimal (base 10), hexadecimal (base 16) and binary
(base 2) values represented as elisp integers, strings and lists, respectively,
e.g. `32`, `"20"`, and `'(0 0 1 0 0 0 0 0)` all represent the same value.

> While Emacs does include [literals for directly specifying hexadecimals and binary
> values](https://www.gnu.org/software/emacs/manual/html_node/elisp/Integer-Basics.html),
> e.g. `#x20` and `#b00100000`, these do unfortunately reduce into their decimal
> representation on evaluation and so aren't as useful for getting a working
> understanding of how hexadecimal and binary values are represented and
> manipulated.

The following constraints are placed on the input and output values throughout
this library:

The standard unit is a byte (8 bits), a value between 0-255, and so if a
negative value or a value above 255 is passed to any function it will have its
valued truncated silently to the size of one byte, unless a specific memory size
is given to the function. The preferred way to produce values above 255 is to
use one of the `b-dec-parse-little-endian` or `b-dec-parse-big-endian` functions
for constructing
[optionally signed](https://en.wikipedia.org/wiki/Signed_number_representations)
8, 16, or 32 bit integers from one or more bytes. Finally, bytes can also be
parsed as floats with the `b-dec-parse-32bit-float`.

When representing hexadecimal values, the library does not use the common `0x`
prefix as this can quickly become noisy when dealing with lists of hexadecimal
values. The length of any computed hexadecimal string representation is always
divisible by 2 to match a whole set of bytes.

Furthermore, the binary list representation has the leftmost bit at index 0 in
contrast to the traditional indexing of bits in a byte from the right. This
choice was made due to readability of the printed result and so please be aware
of this when manipulating a binary list using any regular elisp function that
isn't part of this library. Computed binary list representations are always
divisible by 8 to match a whole set of bytes.

Finally, some of the functions in this library are simple wrappers around
[built-in functions](https://emacsdocs.org/docs/elisp/Bitwise-Operations) but
included here -- often with slight modifications -- for the sake of completeness
of the library.

For further documentation and example function calls, see:
https://github.com/dragonwasrobot/b.el

This library is inspired by https://github.com/magnars/s.el and uses
https://github.com/emacsfodder/etd to generate all example documentation and
tests, which are located in `dev/b-examples.el`.

## Installation

This package is not yet on [Melpa](https://melpa.org), so the easiest way to
install it is to add it to your load path using your preferred method of choice
and call `(require 'b)`.

## Usage

Below are listed all the library functions along with examples.

As this library is aimed at helping to gain an understanding of bytes and their
manipulation, the following helper function might come in handy when evaluating
functions from this library and wanting to have them be inserted directly in
the current buffer:

```elisp
(defun insert-result (value)
  "Insert VALUE into buffer at cursor position."
  (insert (format " ; => %s" value)))
```

Thus, evaluating the following expression directly in your buffer:

```elisp
(insert-result (b-hex-to-dec "3C"))
```

prints the result right next to it:

```elisp
(insert-result (b-hex-to-dec "3C")) ; => 60
```

- - -

[[ function-list ]]

[[ function-docs ]]
