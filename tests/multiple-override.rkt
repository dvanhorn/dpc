#lang class/2
;; Test that methods can be overridden multiple times.
(define-class zero%
  (define (m) 0))

(define-class one%
  (super zero%)
  (define (m) 1))

(define-class two%
  (super one%)
  (define (m) 2))

(check-expect ((new zero%) . m) 0)
(check-expect ((new one%) . m) 1)
(check-expect ((new two%) . m) 2)

   
   