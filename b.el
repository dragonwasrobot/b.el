;;; b.el --- Byte manipulation library -*- lexical-binding: t -*-

;; Copyright (C) 2023 Peter Urbak

;; Author: Peter Urbak <peter@dragonwasrobot.com>
;; URL: https://github.com/dragonwasrobot/b.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (dash "2.19"))
;; Keywords: lisp

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A utility library for parsing and manipulation bytes either as decimals,
;; hexadecimals or binaries. The main purposes of this library is for rapid
;; prototyping of binary protocol and learning about the representation and
;; manipulation of these fundamental building blocks.

;; The library operates on decimal (base 10), hexadecimal (base 16) and binary
;; (base 2) values represented as elisp integers, strings and lists,
;; respectively, e.g. 32, "20", and \\='(0 0 1 0 0 0 0 0) all represent the same
;; value.

;; The following constraints are placed on the input and output values
;; throughout this library:

;; The standard unit is a byte (8 bits), a value between 0-255, and so if a
;; negative value or a value above 255 is passed to any function it will have
;; its valued truncated silently to the size of one byte, unless a specific
;; memory size is given to the function. The preferred way to produce values
;; above 255 is to use one of the `b-dec-parse-little-endian' or
;; `b-dec-parse-big-endian' functions for constructing optionally signed 8, 16,
;; or 32 bit integers from one or more bytes. Finally, bytes can also be parsed
;; as floats with the `b-dec-parse-32bit-float'.

;; When representing hexadecimal values, the library does not use the common
;; '0x' prefix as this can quickly become noisy when dealing with lists of
;; hexadecimal values. The length of any computed hexadecimal string
;; representation is always divisible by 2 to match a whole set of bytes.

;; Furthermore, the binary list representation has the leftmost bit at index 0
;; in contrast to the traditional indexing of bits in a byte from the right.
;; This choice was made due to readability of the printed result and so please
;; be aware of this when manipulating a binary list using any regular elisp
;; function that isn't part of this library. Computed binary list
;; representations are always divisible by 8 to match a whole set of bytes.

;; Finally, some of the functions in this library are simple wrappers around
;; built-in functions but included here -- often with slight modifications -- for
;; the sake of completeness of the library.

;; For further documentation and example function calls, see:
;; https://github.com/dragonwasrobot/b.el

;; This library is inspired by https://github.com/magnars/s.el and uses
;; https://github.com/emacsfodder/etd to generate all example documentation and
;; tests, which are located in `dev/b-examples.el'.

;;; Code:

;; Dependencies

(require 'dash) ; Too damn convenient.

;;; Conversion functions

;;;; Dec <-> Hex

(defun b-dec-to-hex (dec &optional bytes)
  "Return the string representation of the hexadecimal number corresponding to DEC.

The value of DEC gets truncated relative to the total BYTES of
memory. The default value of BYTES is 1 and so DEC must be
between 0 and 255.

A zero is padded onto the left of the result if it has an uneven
length, i.e. 1 becomes 01."
  (declare (pure t) (side-effect-free t))

  (let* ((mem-bytes (if bytes bytes 1))
         (hex-string (format "%X" (b-dec-truncate dec mem-bytes))))
    (if (= 1 (mod (length hex-string) 2))
        (concat "0" hex-string)
      hex-string)))

(defun b-hex-to-dec (hex &optional bytes)
  "Parse HEX as a hexadecimal number and return its decimal representation.

The value of HEX gets truncated relative to the total BYTES of
memory. The default value of BYTES is 1 and so HEX must be
between 00 and FF."
  (declare (pure t) (side-effect-free t))

  (let ((mem-bytes (if bytes bytes 1)))
    (b-dec-truncate (string-to-number hex 16) mem-bytes)))

;;;; Dec <-> Bin

(defun b-dec-to-bin (dec &optional bytes)
  "Return the binary list representation of the decimal DEC.

The value of DEC gets truncated relative to the total BYTES of
memory. The default value of BYTES is 1 and so DEC must be
between 0 and 255.

One or more zeroes are padded onto the left of the resulting list
if the length is not divisible by 8, i.e. \\='(0 1 1 0 1) becomes
\\='(0 0 0 0 1 1 0 1)."
  (declare (pure t) (side-effect-free t))

  (let* ((mem-bytes (if bytes bytes 1))
         (dec (b-dec-truncate dec mem-bytes))

         (bin-list ((lambda () (let ((bin '()))
                            (while (not (= dec 0))
                              (setq bin (cons (if (= 1 (b-dec-and dec 1)) 1 0) bin))
                              (setq dec (ash dec -1)))
                            bin))))

         (length-mod-8 (mod (length bin-list) 8)))

    (if (> length-mod-8 0)
        (append (make-list (- 8 length-mod-8) 0) bin-list)
      bin-list)))

(defun b-bin-to-dec (bin &optional bytes)
  "Parse BIN as a list of bits and return the corresponding decimal number.

The value of BIN gets truncated relative to the total BYTES of
memory. The default value of BYTES is 1 and so BIN must be
between \\='(0 0 0 0 0 0 0 0) and  \\='(1 1 1 1 1 1 1 1)."
  (declare (pure t) (side-effect-free t))

  (let ((mem-bytes (if bytes bytes 1)))
    (--> bin
       (-map #'number-to-string it)
       (mapconcat 'identity it "")
       (b-dec-truncate (string-to-number it 2) mem-bytes))))

;;;; Hex <-> Bin

(defun b-hex-to-bin (hex &optional bytes)
  "Return the binary list representation of the hexadecimal HEX.

The value of HEX gets truncated relative to the total BYTES of
memory. The default value of BYTES is 1 and so DEC must be
between 00 and FF.

One or more zeroes are padded onto the left of the resulting list
if the length is not divisible by 8, i.e. \\='(0 1 1 0 1) becomes
\\='(0 0 0 0 1 1 0 1)."
  (declare (pure t) (side-effect-free t))

  (--> hex
       (b-hex-to-dec it bytes)
       (b-dec-to-bin it bytes)))

(defun b-bin-to-hex (bin &optional bytes)
  "Parse BIN as a list of bits and return the hex string representation.

The value of BIN gets truncated relative to the total BYTES of
memory. The default value of BYTES is 1 and so BIN must be
between \\='(0 0 0 0 0 0 0 0) and  \\='(1 1 1 1 1 1 1 1).

A zero is padded onto the left if the result has an uneven
length, i.e. 1 becomes 01."
  (declare (pure t) (side-effect-free t))

  (--> bin
     (b-bin-to-dec it bytes)
     (b-dec-to-hex it bytes)))

;;;; Collections

(defun b-decs-to-hexs (decs &optional bytes)
  "Return the hex string representation of the list of decimals DECS.

The value of each byte in DECS gets truncated relative to the
total BYTES of memory. The default value of BYTES is 1 and so
each element in DECS must be between 0 and 255.

A zero is padded onto the left of each hexadecimal number in the
result list if the individual value has an uneven length, i.e.
1 becomes 01."
  (declare (pure t) (side-effect-free t))

  (--> decs
       (-map (lambda (dec) (b-dec-to-hex dec bytes)) it)
       (mapconcat 'identity it " ")))

(defun b-hexs-to-decs (hexs &optional bytes)
  "Parse HEXS as a list of hexadecimal numbers and return corresponding decimals.

The value of each byte in HEXS gets truncated relative to the
total BYTES of memory. The default value of BYTES is 1 and so
each element of HEXS must be between 00 and FF."
  (declare (pure t) (side-effect-free t))

  (--> hexs
       (split-string it " ")
       (-map (lambda (hex) (b-hex-to-dec hex bytes)) it)))

;;; Byte manipulation

(defun b-dec-shl (dec n &optional bytes)
  "Shifts decimal DEC N bits to the left, relative to total BYTES of memory.

Omitting BYTES defaults its value to 1 byte of memory."
  (declare (pure t) (side-effect-free t))

  (let ((mem-bytes (if bytes bytes 1)))
    (b-dec-truncate (ash dec n) mem-bytes)))

(defun b-dec-shr (dec n &optional bytes)
  "Shifts decimal DEC N bits to the right, relative to total BYTES of memory.

Omitting BYTES defaults its value to 1 byte of memory."
  (declare (pure t) (side-effect-free t))

  (let ((mem-bytes (if bytes bytes 1)))
    (b-dec-truncate (ash dec (- n)) mem-bytes)))

(defun b-dec-truncate (dec bytes)
  "Truncates the decimal DEC, relative to total BYTES of memory."
  (declare (pure t) (side-effect-free t))

  (let ((max-value (- (expt 2 (* bytes 8)) 1)))
    (b-dec-and dec max-value)))

(defun b-dec-and (&rest decs)
  "Logically ANDs each decimal value of DECS."
  (declare (pure t) (side-effect-free t))

  (apply #'logand decs))

(defun b-dec-or (&rest decs)
  "Logically ORs each decimal value of DECS."
  (declare (pure t) (side-effect-free t))

  (apply #'logior decs))

(defun b-dec-xor (&rest decs)
  "Logically XORs each decimal value of DECS."
  (declare (pure t) (side-effect-free t))

  (apply #'logxor decs))

(defun b-dec-not (dec &optional bytes)
  "Negates the decimal DEC, relative to total BYTES of memory."
  (declare (pure t) (side-effect-free t))

  (--> dec
     (b-dec-to-bin it bytes)
     (-map (lambda (bit) (if (= bit 1) 0 1)) it)
     (b-bin-to-dec it bytes)))

(defun b-hex-shl (hex n &optional bytes)
  "Shifts hexadecimal HEX N bits to the left, relative to total BYTES of memory.

Omitting BYTES defaults its value to 1 byte of memory."
  (declare (pure t) (side-effect-free t))

  (--> hex
     (b-hex-to-dec it bytes)
     (b-dec-shl it n bytes)
     (b-dec-to-hex it bytes)))

(defun b-hex-shr (hex n &optional bytes)
  "Shifts hexadecimal HEX N bits to the right, relative to total BYTES of memory.

Omitting BYTES defaults its value to 1 byte of memory."
  (declare (pure t) (side-effect-free t))

  (--> hex
     (b-hex-to-dec it bytes)
     (b-dec-shr it n bytes)
     (b-dec-to-hex it bytes)))

(defun b-hex-and (&rest hexs)
  "Logically ANDs each hexadecimal value of HEXS."
  (declare (pure t) (side-effect-free t))

  (--> hexs
     (-map (lambda (hex) (b-hex-to-dec hex 4)) it)
     (apply #'b-dec-and it)
     ((lambda (dec) (b-dec-to-hex dec 4)) it)))

(defun b-hex-or (&rest hexs)
  "Logically ORs each hexadecimal value of HEXS."
  (declare (pure t) (side-effect-free t))

  (--> hexs
     (-map (lambda (hex) (b-hex-to-dec hex 4)) it)
     (apply #'b-dec-or it)
     ((lambda (dec) (b-dec-to-hex dec 4)) it)))

(defun b-hex-xor (&rest hexs)
  "Logically XORs each hexadecimal value of HEXS."
  (declare (pure t) (side-effect-free t))

  (--> hexs
       (-map (lambda (hex) (b-hex-to-dec hex 4)) it)
       (apply #'b-dec-xor it)
       ((lambda (dec) (b-dec-to-hex dec 4)) it)))

(defun b-hex-not (hex &optional bytes)
  "Negates the hexadecimal HEX, relative to total BYTES of memory."
  (declare (pure t) (side-effect-free t))

  (--> hex
     (b-hex-to-dec it bytes)
     (b-dec-not it bytes)
     (b-dec-to-hex it bytes)))

;;; Byte parsing integers

(defun b-dec-parse-little-endian (decs &optional bytes signed)
  "Parse a list of decimals DECS as an optionally SIGNED little endian integer.

Each element of DECS should correspond to a single byte with a
integer value between 0 and 255, or it will be truncated.

Parsing is done relative to the total BYTES of memory. The
default value of BYTES is 2, and so DECS will be right-padded
accordingly if less than BYTES in length."
  (declare (pure t) (side-effect-free t))

  (let* ((unsigned-decs (if bytes (b-right-pad-bytes decs bytes) decs))
         (mem-bytes (if bytes bytes 2))
         (truncated-decs (-map (lambda (dec) (b-dec-truncate dec 1))
                               (seq-subseq unsigned-decs 0 mem-bytes)))

         (unsigned-dec (cdr (-reduce-from (lambda (acc dec)
                                            (let* ((bits (car acc))
                                                   (acc-result (cdr acc))
                                                   (new-result (b-dec-shl dec bits mem-bytes)))
                                              (cons (+ 8 bits)
                                                    (+ new-result acc-result))))
                                          (cons 0 0)
                                          truncated-decs)))

         (bytes-count (max (or bytes 0) (length decs)))
         (bytes-max-value (expt 2 (* bytes-count 8))))

    (if (and signed (> unsigned-dec (- (/ bytes-max-value 2) 1)))
        (+ (- bytes-max-value) unsigned-dec)
        unsigned-dec)))

(defun b-dec-parse-big-endian (decs &optional bytes signed)
  "Parse a list of decimals DECS as an optionally SIGNED big endian integer.

Each element of DECS should correspond to a single byte with a
integer value between 0 and 255, or it will be truncated.

Parsing is done relative to the total BYTES of memory. The
default value of BYTES is 2, and so DECS will be left-padded
accordingly if less than BYTES in length."
  (declare (pure t) (side-effect-free t))

  (let* ((unsigned-decs (if bytes (b-left-pad-bytes decs bytes) decs))
         (mem-bytes (if bytes bytes 2))
         (truncated-decs (-map (lambda (dec) (b-dec-truncate dec 1))
                               (seq-subseq unsigned-decs 0 mem-bytes))))

    (b-dec-parse-little-endian (reverse truncated-decs) bytes signed)))

;;; Byte parsing floats

(defun b-dec-parse-32bit-float (decs)
 "Parse DECS into a 32-bit floating point according to the IEEE 754 specification.

DECS should be a list of 4 bytes, formatted according to the
https://en.wikipedia.org/wiki/Single-precision_floating-point_format,
where the first bit of the first byte should be the sign bit."
  (let* ((byte0 (b-dec-to-bin (nth 0 decs)))
         (byte1 (b-dec-to-bin (nth 1 decs)))
         (byte2 (b-dec-to-bin (nth 2 decs)))
         (byte3 (b-dec-to-bin (nth 3 decs)))

         (sign (nth 0 byte0)) ;; bit 31
         (exponent (b-bin-to-dec (append (-drop 1 byte0) (seq-subseq byte1 0 1)))) ;; bits 23-30
         (fraction (cdr (-reduce-from (lambda (acc bit)
                                        (let* ((idx (car acc))
                                               (acc (cdr acc))
                                               (value (* bit (expt 2 (- idx)))))
                                          (cons (+ 1 idx) (+ acc value))))
                                      (cons 1 0)
                                      (append (-drop 1 byte1) byte2 byte3))))) ;; bits 0-22

    (cond
     ((and (eq exponent 255) (= fraction 0)) (* (expt -1 sign) 1.0e+INF))
     ((and (eq exponent 255) (> fraction 0)) (* (expt -1 sign) 0.0e+NaN))

     (t (* (expt -1 sign)
           (expt 2 (- exponent 127))
           (+ 1 fraction))))))

;;; Misc

(defun b-left-pad-bytes (decs bytes)
  "Left pad a list of decimals DECS, relative to total BYTES of memory."
  (declare (pure t) (side-effect-free t))

  (if (< (length decs) bytes)
      (append (make-list (- bytes (length decs)) 0) decs)
    decs))

(defun b-right-pad-bytes (decs bytes)
  "Right pad a list of decimals DECS, relative to total BYTES of memory."
  (declare (pure t) (side-effect-free t))

  (if (< (length decs) bytes)
      (append decs (make-list (- bytes (length decs)) 0))
    decs))

(defun b-sign-bit-set? (dec)
  "Return t if DEC has its sign bit set, nil otherwise."
  (declare (pure t) (side-effect-free t))

  (eq 128 (b-dec-and 128 dec)))

(defun b-set-sign-bit (dec bit)
  "Set sign bit of DEC to BIT."
  (declare (pure t) (side-effect-free t))

  (if (eq bit 1)
      (b-dec-or 128 dec)
      (--> dec (b-dec-shl it 1) (b-dec-shr it 1))))

(provide 'b)

;;; b.el ends here
