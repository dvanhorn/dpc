#lang class5

(define posn%
  (class
    (fields x y)
    (define/public (foo) (+ (x) (y)))))

(define-interface posn<%> (foo) (x y))

(send (new posn% 1 2) foo)

(define cposn%
  (class
      (super posn% posn<%>)
    (fields c)
    (define/public (->list) (list (c) (x) (y) (foo)))))