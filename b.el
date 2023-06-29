;;; b.el --- Byte manipulation library -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Peter Urbak

;; Author: Peter Urbak <peter@dragonwasrobot.com>
;; URL: https://github.com/dragonwasrobot/b.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (dash "2.19"))
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

;; Byte manipulation library for Emacs.
;;
;; See documentation at https://github.com/dragonwasrobot/b.el
;;
;; Inspired by https://github.com/magnars/s.el

;;; Code:

;; Dependencies

(require 'dash) ; Too damn convenient.

;; Dec <-> Hex

(defun b-dec-to-hex (dec)
  "Return the string representation of the hexadecimal number corresponding to DEC.

A zero is padded onto the left if the result has an uneven
length, i.e. \"1\" becomes \"01\". However the result does not
include the common \"0x\" prefix since this quickly becomes
verbose when dealing with a list of hex values.

Examples:

  (b-dec-to-hex 2) ==> \"02\"
  (b-dec-to-hex 16) ==> \"10\"
  (b-dec-to-hex 280) ==> \"0118\""
  (declare (pure t) (side-effect-free t))

  (let ((hex-string (format "%X" dec)))
    (if (= 1 (mod (length hex-string) 2))
        (concat "0" hex-string)
      hex-string)))

(defun b-hex-to-dec (hex)
  "Parse HEX as a hexadecimal number and return the decimal representation.

Examples:

  (b-hex-to-dec \"10\") ==> 16
  (b-hex-to-dec \"118\") ==> 280
  (b-hex-to-dec \"0118\") ==> 280
  (b-hex-to-dec \"1010\") ==> 4112."
  (declare (pure t) (side-effect-free t))

  (string-to-number hex 16))

;; Dec <-> Bin

(defun b-dec-to-bin (dec)
  "Return the binary list representation of the decimal DEC.

One or more zeroes are padded onto the left of the resulting list
if the length is not divisible by 8, i.e. \\='(0 1 1 0 1) becomes
\\='(0 0 0 0 1 1 0 1).

TODO: Note that bits are usually indexed from the right so be
aware of this when using the `bin' representation directly.

Examples:

  (b-dec-to-bin 17) ==> \\='(0 0 0 1 0 0 0 1)"
  (declare (pure t) (side-effect-free t))

  (let* ((bin-list ((lambda () (let ((bin '()))
                                 (while (not (= dec 0))
                                   (setq bin (cons (if (= 1 (logand dec 1)) 1 0) bin))
                                   (setq dec (ash dec -1)))
                                 bin))))

         (length-mod-8 (mod (length bin-list) 8)))
    (if (> length-mod-8 0)
        (append (make-list (- 8 length-mod-8) 0) bin-list)
      bin-list)))

(defun b-bin-to-dec (bin)
  "Parse BIN as a list of bits and return the corresponding decimal number.

Examples:

  (b-bin-to-dec \\='(1 1 0 1)) ==> 13
  (b-bin-to-dec \\='(1 0 1 0 1)) ==> 21"
  (declare (pure t) (side-effect-free t))

  (--> bin
     (mapcar #'number-to-string it)
     (string-join it "")
     (string-to-number it 2)))

;; Bin <-> Hex

(defun b-bin-to-hex (bin)
  "Parse BIN as a list of bits and return the hex string representation.

A zero is padded onto the left if the result has an uneven
length, i.e. \"1\" becomes \"01\". However the result does not
include the common \"0x\" prefix since this quickly becomes
verbose when dealing with a list of hex values.

Examples:

  (b-bin-to-hex \\='(1 0 1 0 0 0)) ==> \"28\"
  (b-bin-to-hex \\='(1 0 0 0 1 1 0 0 0)) ==> \"0118\""
  (declare (pure t) (side-effect-free t))

  (->> bin
     b-bin-to-dec
     b-dec-to-hex))

(defun b-hex-to-bin (hex)
  "Return the binary list representation of the hexadecimal HEX.

One or more zeroes are padded onto the left of the resulting list
if the length is not divisible by 4, i.e. \\='(0 1 1 0 1) becomes
\\='(0 0 0 0 1 1 0 1).

Examples:

  (b-hex-to-bin \"0D\") ==> \\='(1 1 0 1)
  (b-hex-to-bin \"11\") ==> \\='(0 0 0 1 0 0 0 1)
  (b-hex-to-bin \"118\") ==> \\='(0 0 0 1 0 0 0 1 1 0 0 0)"
  (declare (pure t) (side-effect-free t))

  (->> hex
     b-hex-to-dec
     b-dec-to-bin))

;; Collection functions

(defun b-decs-to-hexs (decs)
  "Return the hex string representation of the list of decimals DECS.

A zero is padded onto the left of each hexadecimal number if the
result has an uneven length, i.e. \"1\" becomes \"01\". However
the result does not include the common \"0x\" prefix since this
quickly becomes verbose when dealing with a list of hex values.

Examples:

  (b-decs-to-hexs \\='(19 4 130 3 25 3 201 2 190 2 19 3 155 3 8 4 52))
  ==> \"13 04 82 03 19 03 C9 02 BE 02 13 03 9B 03 08 04 34\""
  (declare (pure t) (side-effect-free t))

  (--> decs
       (mapcar #'b-dec-to-hex it)
       (string-join it " ")))

(defun b-hexs-to-decs (hexs)
  "Parse HEXS as a list of hexadecimal numbers and return corresponding decimals.

Examples:

  (b-hexs-to-decs \"13 04 82 03 19 03 C9 02 BE 02 13 03 9B 03 08 04 34\")
  ==> \\='(19 4 130 3 25 3 201 2 190 2 19 3 155 3 8 4 52)"
  (declare (pure t) (side-effect-free t))

  (--> hexs
       (string-split it " ")
       (mapcar #'b-hex-to-dec it)))

;; Bit manipulation (decimal)

(defun b-dec-shl (dec count &optional bytes)
  "Shifts the decimal DEC COUNT bits to the left, relative to BYTES of memory.

Omitting BYTES defaults to an uint32 interpretation.

Examples:

  (b-dec-shl 4 1) ==> 8
  (b-dec-shl 1 8) ==> 256"
  (declare (pure t) (side-effect-free t))

  (let ((mask (if bytes (- (ash bytes 8) 1) (- (ash 1 32) 1))))
    (logand mask (ash dec count))))

(defun b-dec-shr (dec count &optional bytes)
  "Shifts the decimal DEC COUNT bits to the right, relative to BYTES of memory.

Omitting BYTES defaults to an uint32 interpretation.

Examples:

  (b-dec-shr 4 1) ==> 2
  (b-dec-shr 256 8) ==> 1"
  (declare (pure t) (side-effect-free t))

  (let ((mask (if bytes (- (ash bytes 8) 1) (- (ash 1 32) 1))))
    (logand mask (ash dec (- count)))))

(defalias 'b-dec-and #'logand "TODO")

(defalias 'b-dec-or #'logior "TODO")

(defalias 'b-dec-xor #'logxor "TODO")

(defun b-dec-not (dec)
  "TODO

Examples:

  (b-dec-not 60) ==> 195"
  (declare (pure t) (side-effect-free t))

  (->> dec
     b-dec-to-bin
     (mapcar (lambda (bit) (if (= bit 1) 0 1)))
     b-bin-to-dec))

;; Bit manipulation (hexadecimal)

(defun b-hex-shl (hex count &optional bytes)
  "Shifts the hexadecimal HEX COUNT bits to the left, relative to BYTES of memory.

Omitting BYTES defaults to an uint32 interpretation.

Examples:

  (b-hex-shl \"0F\" 4) ==> \"FO\"
  (b-hex-shl \"F2\" 8) ==> \"F200\""
  (declare (pure t) (side-effect-free t))

  (--> hex
     (b-hex-to-dec it)
     (b-dec-shl it count bytes)
     (b-dec-to-hex it)))

(defun b-hex-shr (hex count &optional bytes)
  "Shifts the hexadecimal HEX COUNT bits to the right, relative to BYTES of memory.

Omitting BYTES defaults to an uint32 interpretation.

Examples:

  (b-hex-shr \"0AF0\" 8) ==> \"0A\"
  (b-hex-shr \"2F\" 4) ==> \"02\""
  (declare (pure t) (side-effect-free t))

  (--> hex
       (b-hex-to-dec it)
       (b-dec-shr it count bytes)
       (b-dec-to-hex it)))

(defun b-hex-and (&rest hexs)
  "TODO

Examples:

  (b-hex-and \"A0\" \"AF\") ==> 160"
  (declare (pure t) (side-effect-free t))

  (->> hexs
     (mapcar #'b-hex-to-dec)
     (apply #'b-dec-and)))

(defun b-hex-or (&rest hexs)
  "TODO

Examples:

  (b-hex-or \"A0\" \"AF\") ==> 175"
  (declare (pure t) (side-effect-free t))

  (->> hexs
       (mapcar #'b-hex-to-dec)
       (apply #'b-dec-or)))

(defun b-hex-xor (&rest hexs)
  "TODO

Examples:

  (b-hex-xor \"A0\" \"AF\") ==> 15"
  (declare (pure t) (side-effect-free t))

  (->> hexs
       (mapcar #'b-hex-to-dec)
       (apply #'b-dec-xor)))

(defun b-hex-not (hex)
  "TODO

Examples:

  (b-hex-not \"3C\") ==> \"C3\""
  (declare (pure t) (side-effect-free t))

  (->> hex
       b-hex-to-dec
       b-dec-not
       b-dec-to-hex))

;; Byte parsing

(defun b-left-pad-bytes (decs bytes)
  "TODO

  (b-left-pad-bytes '(1 2) 4) ;; '(0 0 1 2)"
  (declare (pure t) (side-effect-free t))

  (if (< (length decs) bytes)
      (append (make-list (- bytes (length decs)) 0) decs)
    decs))

(defun b-right-pad-bytes (decs bytes)
  "TODO

  (b-left-pad-bytes '(1 2) 4) ;; '(0 0 1 2)"
  (declare (pure t) (side-effect-free t))

  (if (< (length decs) bytes)
      (append decs (make-list (- bytes (length decs)) 0))
    decs))

(defun b-sign-bit-set? (dec)
  "TODO

  (b-sign-bit-set? (car (last '(1 2 3)))) ;; nil"
  (declare (pure t) (side-effect-free t))

  (eq 1 (nth 0 (b-dec-to-bin dec))))

(defun b-set-sign-bit (dec bit)
  "TODO

  (b-set-sign-bit 42 1) ;; 170"
  (declare (pure t) (side-effect-free t))

  (->> dec
     b-dec-to-bin
     cdr
     (cons bit)
     (b-bin-to-dec)))

(defun b-dec-parse-little-endian (decs &optional bytes signed)
  "TODO

  (b-dec-parse-little-endian '(127) 1 nil) ;; 127
  (b-dec-parse-little-endian '(128) 1 t) ;; -128
  (b-dec-parse-little-endian '(128) 2 t) ;; 128
  (b-dec-parse-little-endian '(0 128) nil t) ;; -32768
  (b-dec-parse-little-endian '(0 128) 3 t) ;; 32768"
  (declare (pure t) (side-effect-free t))

  (let* ((unsigned-decs (if bytes (b-right-pad-bytes decs bytes) decs))

         (unsigned-dec (cdr (seq-reduce (lambda (acc dec)
                                          (let* ((bits (car acc))
                                                 (acc-result (cdr acc))
                                                 (new-result (b-dec-shl dec bits)))
                                            (cons (+ 8 bits)
                                                  (+ new-result acc-result))))
                                        unsigned-decs
                                        (cons 0 0))))

         (bytes-count (max (or bytes 0) (length decs)))
         (bytes-max-value (expt 2 (* bytes-count 8))))

    (if (and signed (> unsigned-dec (- (/ bytes-max-value 2) 1)))
        (+ (- bytes-max-value) unsigned-dec)
        unsigned-dec)))

(defun b-dec-parse-big-endian (decs &optional bytes signed)
  "TODO

  (b-dec-parse-big-endian '(127) 1 nil) ;; 127
  (b-dec-parse-big-endian '(128) 1 t) ;; -128
  (b-dec-parse-big-endian '(128) 2 t) ;; 128
  (b-dec-parse-big-endian '(128 0) nil t) ;; -32768
  (b-dec-parse-big-endian '(128 0) 3 t) ;; 32768"
  (declare (pure t) (side-effect-free t))

  (b-dec-parse-little-endian (reverse decs) bytes signed))

;; Misc

(defun b-insert (value)
  "TODO"
  (insert (format "%s" value)))

(provide 'b)

;;; b.el ends here
