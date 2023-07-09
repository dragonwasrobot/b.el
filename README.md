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


### Conversion functions

* [b-dec-to-hex](#b-dec-to-hex-dec-optional-bytes) `(dec &optional bytes)`
* [b-hex-to-dec](#b-hex-to-dec-hex-optional-bytes) `(hex &optional bytes)`
* [b-dec-to-bin](#b-dec-to-bin-dec-optional-bytes) `(dec &optional bytes)`
* [b-bin-to-dec](#b-bin-to-dec-bin-optional-bytes) `(bin &optional bytes)`
* [b-hex-to-bin](#b-hex-to-bin-hex-optional-bytes) `(hex &optional bytes)`
* [b-bin-to-hex](#b-bin-to-hex-bin-optional-bytes) `(bin &optional bytes)`
* [b-decs-to-hexs](#b-decs-to-hexs-decs-optional-bytes) `(decs &optional bytes)`
* [b-hexs-to-decs](#b-hexs-to-decs-hexs-optional-bytes) `(hexs &optional bytes)`

### Byte manipulation

* [b-dec-shl](#b-dec-shl-dec-n-optional-bytes) `(dec n &optional bytes)`
* [b-dec-shr](#b-dec-shr-dec-n-optional-bytes) `(dec n &optional bytes)`
* [b-dec-and](#b-dec-and-rest-decs) `(&rest decs)`
* [b-dec-or](#b-dec-or-rest-decs) `(&rest decs)`
* [b-dec-xor](#b-dec-xor-rest-decs) `(&rest decs)`
* [b-dec-not](#b-dec-not-dec-optional-bytes) `(dec &optional bytes)`
* [b-hex-shl](#b-hex-shl-hex-n-optional-bytes) `(hex n &optional bytes)`
* [b-hex-shr](#b-hex-shr-hex-n-optional-bytes) `(hex n &optional bytes)`
* [b-hex-and](#b-hex-and-rest-hexs) `(&rest hexs)`
* [b-hex-or](#b-hex-or-rest-hexs) `(&rest hexs)`
* [b-hex-xor](#b-hex-xor-rest-hexs) `(&rest hexs)`
* [b-hex-not](#b-hex-not-hex-optional-bytes) `(hex &optional bytes)`

### Byte parsing

* [b-dec-parse-little-endian](#b-dec-parse-little-endian-decs-optional-bytes-signed) `(decs &optional bytes signed)`
* [b-dec-parse-big-endian](#b-dec-parse-big-endian-decs-optional-bytes-signed) `(decs &optional bytes signed)`
* [b-dec-parse-32bit-float](#b-dec-parse-32bit-float-decs) `(decs)`

### Misc

* [b-left-pad-bytes](#b-left-pad-bytes-decs-bytes) `(decs bytes)`
* [b-right-pad-bytes](#b-right-pad-bytes-decs-bytes) `(decs bytes)`
* [b-sign-bit-set?](#b-sign-bit-set-dec) `(dec)`
* [b-set-sign-bit](#b-set-sign-bit-dec-bit) `(dec bit)`


### b-dec-to-hex `(dec &optional bytes)`

Return the string representation of the hexadecimal number corresponding to `dec`.

The value of `dec` gets truncated relative to the total `bytes` of
memory. The default value of `bytes` is 1 and so `dec` must be
between 0 and 255.

`a` zero is padded onto the left of the result if it has an uneven
length, i.e. 1 becomes 01.

```lisp
(b-dec-to-hex 2)
 ⇒ "02"
(b-dec-to-hex 26)
 ⇒ "1A"
(b-dec-to-hex 235)
 ⇒ "EB"
```

### b-hex-to-dec `(hex &optional bytes)`

Parse `hex` as a hexadecimal number and return its decimal representation.

The value of `hex` gets truncated relative to the total `bytes` of
memory. The default value of `bytes` is 1 and so `hex` must be
between 00 and `ff`.

```lisp
(b-hex-to-dec "02")
 ⇒ 2
(b-hex-to-dec "1A")
 ⇒ 26
(b-hex-to-dec "6C")
 ⇒ 108
```

### b-dec-to-bin `(dec &optional bytes)`

Return the binary list representation of the decimal `dec`.

The value of `dec` gets truncated relative to the total `bytes` of
memory. The default value of `bytes` is 1 and so `dec` must be
between 0 and 255.

One or more zeroes are padded onto the left of the resulting list
if the length is not divisible by 8, i.e. '(0 1 1 0 1) becomes
'(0 0 0 0 1 1 0 1).

```lisp
(b-dec-to-bin 17)
 ⇒ '(0 0 0 1 0 0 0 1)
```

### b-bin-to-dec `(bin &optional bytes)`

Parse `bin` as a list of bits and return the corresponding decimal number.

The value of `bin` gets truncated relative to the total `bytes` of
memory. The default value of `bytes` is 1 and so `bin` must be
between '(0 0 0 0 0 0 0 0) and  '(1 1 1 1 1 1 1 1).

```lisp
(b-bin-to-dec '(1 1 0 1))
 ⇒ 13
(b-bin-to-dec '(1 0 1 0 1))
 ⇒ 21
```

### b-hex-to-bin `(hex &optional bytes)`

Return the binary list representation of the hexadecimal `hex`.

The value of `hex` gets truncated relative to the total `bytes` of
memory. The default value of `bytes` is 1 and so `dec` must be
between 00 and `ff`.

One or more zeroes are padded onto the left of the resulting list
if the length is not divisible by 8, i.e. '(0 1 1 0 1) becomes
'(0 0 0 0 1 1 0 1).

```lisp
(b-hex-to-bin "0D")
 ⇒ '(0 0 0 0 1 1 0 1)
(b-hex-to-bin "11")
 ⇒ '(0 0 0 1 0 0 0 1)
(b-hex-to-bin "118" 2)
 ⇒ '(0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 0)
```

### b-bin-to-hex `(bin &optional bytes)`

Parse `bin` as a list of bits and return the hex string representation.

The value of `bin` gets truncated relative to the total `bytes` of
memory. The default value of `bytes` is 1 and so `bin` must be
between '(0 0 0 0 0 0 0 0) and  '(1 1 1 1 1 1 1 1).

`a` zero is padded onto the left if the result has an uneven
length, i.e. 1 becomes 01.

```lisp
(b-bin-to-hex '(0 0 1 0 1 0 0 0))
 ⇒ "28"
(b-bin-to-hex '(1 0 1 1 0 1 0 0))
 ⇒ "B4"
```

### b-decs-to-hexs `(decs &optional bytes)`

Return the hex string representation of the list of decimals `decs`.

The value of each byte in `decs` gets truncated relative to the
total `bytes` of memory. The default value of `bytes` is 1 and so
each element in `decs` must be between 0 and 255.

`a` zero is padded onto the left of each hexadecimal number in the
result list if the individual value has an uneven length, i.e.
1 becomes 01.

```lisp
(b-decs-to-hexs '(19 4 130 3 25 3 201 2 190 2 19 3 155 3 8 4 52))
 ⇒ "13 04 82 03 19 03 C9 02 BE 02 13 03 9B 03 08 04 34"
```

### b-hexs-to-decs `(hexs &optional bytes)`

Parse `hexs` as a list of hexadecimal numbers and return corresponding decimals.

The value of each byte in `hexs` gets truncated relative to the
total `bytes` of memory. The default value of `bytes` is 1 and so
each element of `hexs` must be between 00 and `ff`.

```lisp
(b-hexs-to-decs "13 04 82 03 19 03 C9 02 BE 02 13 03 9B 03 08 04 34")
 ⇒ '(19 4 130 3 25 3 201 2 190 2 19 3 155 3 8 4 52)
```


### b-dec-shl `(dec n &optional bytes)`

Shifts decimal `dec` `n` bits to the left, relative to total `bytes` of memory.

Omitting `bytes` defaults its value to 1 byte of memory.

```lisp
(b-dec-shl 4 1)
 ⇒ 8
(b-dec-shl 1 8 2)
 ⇒ 256
```

### b-dec-shr `(dec n &optional bytes)`

Shifts decimal `dec` `n` bits to the right, relative to total `bytes` of memory.

Omitting `bytes` defaults its value to 1 byte of memory.

```lisp
(b-dec-shr 4 1)
 ⇒ 2
(b-dec-shr 256 8)
 ⇒ 1
```

### b-dec-and `(&rest decs)`

Logically ANDs each decimal value of `decs`.

```lisp
(b-dec-and 17 84)
 ⇒ 16
```

### b-dec-or `(&rest decs)`

Logically ORs each decimal value of `decs`.

```lisp
(b-dec-or 17 84)
 ⇒ 85
```

### b-dec-xor `(&rest decs)`

Logically XORs each decimal value of `decs`.

```lisp
(b-dec-xor 17 84)
 ⇒ 69
```

### b-dec-not `(dec &optional bytes)`

Negates the decimal `dec`, relative to total `bytes` of memory.

```lisp
(b-dec-not 60)
 ⇒ 195
```

### b-hex-shl `(hex n &optional bytes)`

Shifts hexadecimal `hex` `n` bits to the left, relative to total `bytes` of memory.

Omitting `bytes` defaults its value to 1 byte of memory.

```lisp
(b-hex-shl "0F" 4)
 ⇒ "F0"
(b-hex-shl "F2" 8 2)
 ⇒ "F200"
```

### b-hex-shr `(hex n &optional bytes)`

Shifts hexadecimal `hex` `n` bits to the right, relative to total `bytes` of memory.

Omitting `bytes` defaults its value to 1 byte of memory.

```lisp
(b-hex-shr "0AF0" 8 2)
 ⇒ "0A"
(b-hex-shr "2F" 4)
 ⇒ "02"
```

### b-hex-and `(&rest hexs)`

Logically ANDs each hexadecimal value of `hexs`.

```lisp
(b-hex-and "AE" "BE")
 ⇒ "AE"
```

### b-hex-or `(&rest hexs)`

Logically ORs each hexadecimal value of `hexs`.

```lisp
(b-hex-or "A0" "AF")
 ⇒ "AF"
```

### b-hex-xor `(&rest hexs)`

Logically XORs each hexadecimal value of `hexs`.

```lisp
(b-hex-xor "A0" "AF")
 ⇒ "0F"
```

### b-hex-not `(hex &optional bytes)`

Negates the hexadecimal `hex`, relative to total `bytes` of memory.

```lisp
(b-hex-not "3C")
 ⇒ "C3"
```


### b-dec-parse-little-endian `(decs &optional bytes signed)`

Parse a list of decimals `decs` as an optionally `signed` little endian integer.

Each element of `decs` should correspond to a single byte with a
integer value between 0 and 255, or it will be truncated.

Parsing is done relative to the total `bytes` of memory. The
default value of `bytes` is 2, and so `decs` will be right-padded
accordingly if less than `bytes` in length.

```lisp
(b-dec-parse-little-endian '(102 3) 2 nil)
 ⇒ 870
(b-dec-parse-little-endian '(154 252) 2 t)
 ⇒ -870
(b-dec-parse-little-endian '(127) 1 t)
 ⇒ 127
```

### b-dec-parse-big-endian `(decs &optional bytes signed)`

Parse a list of decimals `decs` as an optionally `signed` big endian integer.

Each element of `decs` should correspond to a single byte with a
integer value between 0 and 255, or it will be truncated.

Parsing is done relative to the total `bytes` of memory. The
default value of `bytes` is 2, and so `decs` will be left-padded
accordingly if less than `bytes` in length.

```lisp
(b-dec-parse-big-endian '(3 102) 2 nil)
 ⇒ 870
(b-dec-parse-big-endian '(252 154) 2 t)
 ⇒ -870
(b-dec-parse-big-endian '(127) 1 nil)
 ⇒ 127
```

### b-dec-parse-32bit-float `(decs)`

Parse `decs` into a 32-bit floating point according to the `ieee` 754 specification.

`decs` should be a list of 4 bytes, formatted according to the
https://en.wikipedia.org/wiki/Single-precision_floating-point_format,
where the first bit of the first byte should be the sign bit.

```lisp
(b-dec-parse-32bit-float '(62 32 0 0))
 ⇒ 0.15625
(b-dec-parse-32bit-float '(255 128 0 0))
 ⇒ -1.0e+INF
(b-dec-parse-32bit-float '(127 176 16 0))
 ⇒ 0.0e+NaN
```


### b-left-pad-bytes `(decs bytes)`

Left pad a list of decimals `decs`, relative to total `bytes` of memory.

```lisp
(b-left-pad-bytes '(1 2) 4)
 ⇒ '(0 0 1 2)
```

### b-right-pad-bytes `(decs bytes)`

Right pad a list of decimals `decs`, relative to total `bytes` of memory.

```lisp
(b-right-pad-bytes '(1 2 0 0) 4)
 ⇒ '(1 2 0 0)
```

### b-sign-bit-set? `(dec)`

Return t if `dec` has its sign bit set, nil otherwise.

```lisp
(b-sign-bit-set? 39)
 ⇒ nil
(b-sign-bit-set? 129)
 ⇒ t
```

### b-set-sign-bit `(dec bit)`

Set sign bit of `dec` to `bit`.

```lisp
(b-set-sign-bit 42 0)
 ⇒ 42
(b-set-sign-bit 42 1)
 ⇒ 170
(b-set-sign-bit 170 0)
 ⇒ 42
```

