#lang class1

(define-class x% (fields y))
((new x% (new x% 5)) . y . y)

(define-interface x (a b))
(define-interface y (super x) (c))

(define-class g
  (implements y)
  (fields c a b)
  )