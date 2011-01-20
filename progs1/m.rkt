(module m class1
  
  (define-class a%    
    (fields x)
    (define/public (inc) (add1 (field x))))
  
  
  (define-class b%
    (super a%)
    (fields y)
    (define/public (f z)
      (list 'parent (field x) 'child (inc) z)))
  
  (send (new b% 1 2) f 3)
  (send (new b% 1 2) inc))


