#lang class1

(define-interface foo (inc))

(define-class a%    
  (implements foo)
  (fields x)  
  (define/public (inc) (add1 (field x))))


(define-class b%
  (super a%)
  (fields y)
  (define/public (f z)
    (list 'parent (field x) 'child (inc) z)))

(send (new b% 1 2) f 3)
(send (new b% 1 2) inc)


