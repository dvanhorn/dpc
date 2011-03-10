#lang class4

(define-class point%
  (fields x y)
  (define/public (mv x y)
    (point% x y)))

(define-class color-point%
  (super point%)
  (fields c)
  
  (define/public (mv x y)
    (color-point% x y (field c))))

(define cp (color-point% "red" 1 2))
