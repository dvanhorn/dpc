#lang class/0
;; Test that check-expects can appear within class definitions.
;; See https://github.com/dvanhorn/dpc/issues/6
(define-class check-check%
  (check-expect seven 7)
  (check-error (/ 1 0) "/: division by zero")
  (check-within #i7 seven 0.001)
  (check-member-of seven 6 7 8 9 10)
  (check-range seven 6 8))

(define seven 7)

   
   