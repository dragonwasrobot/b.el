;;; b-examples --- tests for b.el -*- lexical-binding: t; -*-

;; Dependencies

(require 'etd)
(require 'b)

(etd-group "Conversion functions"
  (etd-examples b-dec-to-hex
    (b-dec-to-hex 2) => "02"
    (b-dec-to-hex 16) => "10"
    (b-dec-to-hex 280) => "0118"))

;; (etd-group "Bit manipulation"
;;            )

;; (etd-group "Byte parsing"
;;            )


;;; b-examples.el ends here
