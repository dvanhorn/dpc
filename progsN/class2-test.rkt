#lang class2

(define-class c%
  (fields x y z))

(new c% 4 5 3)

(define-class c2%
  (fields x y z)
  (constructor (a)
               (fields a (* a a) (- a))))

(new c2% 17)