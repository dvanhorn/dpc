#lang class3

(define-class a%
  (fields x)
  (define/public (x! v) 
    (set-field! x v)))

(define a (a% 1))

a


(a . x! 5)

a

