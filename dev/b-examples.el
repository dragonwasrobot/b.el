;;; b-examples --- tests for b.el -*- lexical-binding: t; -*-
;;
;; Version: 0.1.0
;;
;;; Commentary:
;;
;;; Code:
;;
;; Local Variables:
;; no-byte-compile: t
;; End:

;; Dependencies

(require 'etd)
(require 'b)

(etd-group "Conversion functions"

  ;; Dec <-> Hex
  (etd-examples b-dec-to-hex
    (b-dec-to-hex 2) => "02"
    (b-dec-to-hex 26) => "1A"
    (b-dec-to-hex 235) => "EB"
    (b-dec-to-hex 364) => "6C") ; truncated

  (etd-examples b-hex-to-dec
    (b-hex-to-dec "02") => 2
    (b-hex-to-dec "1A") => 26
    (b-hex-to-dec "6C") => 108
    (b-hex-to-dec "016C") => 108) ; truncated

  ;; Dec <-> Bin
  (etd-examples b-dec-to-bin
    (b-dec-to-bin 17) => '(0 0 0 1 0 0 0 1))

  (etd-examples b-bin-to-dec
    (b-bin-to-dec '(1 1 0 1)) => 13
    (b-bin-to-dec '(1 0 1 0 1)) => 21)

  ;; Hex <-> Bin
  (etd-examples b-hex-to-bin
    (b-hex-to-bin "0D") => '(0 0 0 0 1 1 0 1)
    (b-hex-to-bin "11") => '(0 0 0 1 0 0 0 1)
    (b-hex-to-bin "118" 2) => '(0 0 0 0 0 0 0 1 0 0 0 1 1 0 0 0))

  (etd-examples b-bin-to-hex
    (b-bin-to-hex '(0 0 1 0 1 0 0 0)) => "28"
    (b-bin-to-hex '(1 0 1 1 0 1 0 0)) => "B4")

  ;; Collections
  (etd-examples b-decs-to-hexs
    (b-decs-to-hexs '(19 4 130 3 25 3 201 2 190 2 19 3 155 3 8 4 52)) =>
   "13 04 82 03 19 03 C9 02 BE 02 13 03 9B 03 08 04 34")

  (etd-examples b-hexs-to-decs
    (b-hexs-to-decs "13 04 82 03 19 03 C9 02 BE 02 13 03 9B 03 08 04 34") =>
    '(19 4 130 3 25 3 201 2 190 2 19 3 155 3 8 4 52)))

(etd-group "Byte manipulation"
  (etd-examples b-dec-shl
    (b-dec-shl 4 1) => 8
    (b-dec-shl 1 8 2) => 256)

  (etd-examples b-dec-shr
    (b-dec-shr 4 1) => 2
    (b-dec-shr 256 8) => 1)

  (etd-examples b-dec-and
    (b-dec-and 17 84) => 16)

  (etd-examples b-dec-or
    (b-dec-or 17 84) => 85)

  (etd-examples b-dec-xor
    (b-dec-xor 17 84) => 69)

  (etd-examples b-dec-not
    (b-dec-not 60) => 195)

  (etd-examples b-hex-shl
    (b-hex-shl "0F" 4) => "F0"
    (b-hex-shl "F2" 8 2) => "F200")

  (etd-examples b-hex-shr
    (b-hex-shr "0AF0" 8 2) => "0A"
    (b-hex-shr "2F" 4) => "02")

  (etd-examples b-hex-and
    (b-hex-and "AE" "BE") => "AE")

  (etd-examples b-hex-or
    (b-hex-or "A0" "AF") => "AF")

  (etd-examples b-hex-xor
    (b-hex-xor "A0" "AF") => "0F")

  (etd-examples b-hex-not
    (b-hex-not "3C") => "C3"))

(etd-group "Byte parsing"

  (etd-examples b-dec-parse-little-endian
    (b-dec-parse-little-endian '(102 3) 2 nil) => 870
    (b-dec-parse-little-endian '(154 252) 2 t) => -870
    (b-dec-parse-little-endian '(127) 1 t) => 127
    (b-dec-parse-little-endian '(128) 1 t) => -128
    (b-dec-parse-little-endian '(128) 2 t) => 128
    (b-dec-parse-little-endian '(0 128) nil t) => -32768
    (b-dec-parse-little-endian '(0 128) 3 t) => 32768)

  (etd-examples b-dec-parse-big-endian
    (b-dec-parse-big-endian '(3 102) 2 nil) => 870
    (b-dec-parse-big-endian '(252 154) 2 t) => -870
    (b-dec-parse-big-endian '(127) 1 nil) => 127
    (b-dec-parse-big-endian '(128) 1 t) => -128
    (b-dec-parse-big-endian '(128) 2 t) => 128
    (b-dec-parse-big-endian '(128 0) nil t) => -32768
    (b-dec-parse-big-endian '(128 0) 3 t) => 32768)

  (etd-examples b-dec-parse-32bit-float
    (b-dec-parse-32bit-float '(62 32 0 0)) => 0.15625
    (b-dec-parse-32bit-float '(255 128 0 0)) => -1.0e+INF
    (b-dec-parse-32bit-float '(127 176 16 0)) => 0.0e+NaN))

(etd-group "Misc"
  (etd-examples b-left-pad-bytes
    (b-left-pad-bytes '(1 2) 4) => '(0 0 1 2))

  (etd-examples b-right-pad-bytes
    (b-right-pad-bytes '(1 2 0 0) 4) => '(1 2 0 0))

  (etd-examples b-sign-bit-set?
    (b-sign-bit-set? 39) => nil
    (b-sign-bit-set? 129) => t)

  (etd-examples b-set-sign-bit
    (b-set-sign-bit 42 0) => 42
    (b-set-sign-bit 42 1) => 170
    (b-set-sign-bit 170 0) => 42
    (b-set-sign-bit 170 1) => 170))

;;; b-examples.el ends here
