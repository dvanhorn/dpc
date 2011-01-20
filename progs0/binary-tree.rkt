#lang class0
; A BT is one of:
; - (new leaf% Number)
; - (new node% Number BT BT)

(define-class leaf%
  (fields number)
  ; -> Number
  ; count the number of numbers in this leaf
  (define/public (count)
    1)
  ; Number -> BT
  ; double the leaf and put the number on top
  (define/public (double n)
    (new node% n this this)))

(define-class node%
  (fields number left right)
  ; -> Number
  ; count the number of numbers in this node
  (define/public (count)
    (+ 1
       (send (field left) count)
       (send (field right) count)))
  ; Number -> BT
  ; double the node and put the number on top
  (define/public (double n)
    (new node% n this this)))

(define ex1 (new leaf% 7))
(define ex2 (new node% 6
                 (new leaf% 8)
                 (new node% 4
                      (new leaf% 3)
                      (new leaf% 2))))
(define ex3 (new node% 8
                 (new leaf% 2)
                 (new leaf% 1)))

(check-expect (send ex1 count) 1)
(check-expect (send ex2 count) 5)
(check-expect (send ex3 count) 3)

(check-expect (send ex1 double 5)
              (new node% 5 ex1 ex1))
(check-expect (send ex3 double 0)
              (new node% 0 ex3 ex3))