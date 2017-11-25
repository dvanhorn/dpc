#lang class/5

(define-interface posn<%> (foo) ())
(define (add-color %)
  (class (name cposn%)
    (super % posn<%>)
    (define/public (foo) (super))))
