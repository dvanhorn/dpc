#lang class5

(define posn%
  (class
    (fields x y)
    (define/public (foo) (+ (x) (y)))))

(define-interface posn<%> (foo) (x y))

(send (posn% 1 2) foo)

(define (add-color %)
  (class (name cposn%)
    (super % posn<%>)
    (fields c)
    (define/public (foo) (+ (super) (c)))
    (define/public (->list) (list (c) (x) (y) (foo)))))

(define cposn% (add-color posn%))

(check-expect ((cposn% 1 2 3) . ->list) '(1 2 3 6))
(cposn% 1 2 3)

;; key mixin examples
;; - add bounding box
;; - add ordering methods based on comparator
;; - door examples/fish examples



(define-interface none<%> [])

(define (two-mixin %)
 (class (super % none<%>)
   (fields d)))

(define one%
 (two-mixin
  (class
      (fields a b c))))

(define o (one% 'd 'a 'b 'c))
(o . a) ; 'a
(o . b) ; 'b
(o . c) ; 'c
(o . d) ; 'd
o       ; (object:class% 'd)