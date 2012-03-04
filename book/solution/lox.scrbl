#lang scribble/manual
@(require class/utils
          (for-label (except-in class/0 check-expect length
                                cons append reverse map filter foldl foldr first rest))
          (for-label (only-in lang/htdp-intermediate-lambda check-expect)))

@title[#:tag "Parametric_Lists_solution"]{Solution:
@secref{Parametric_Lists}}

This is a solution for the @secref{Parametric_Lists} exercise.

@codeblock{
#lang class/0
;; ==========================================================
;; Lists of X

;; INTERFACE
;; ---------
;; A [List X] implements:
;;
;; - cons : X -> [List X]
;;   Cons given value on to this list.
;;
;; - length : -> Natural
;;   Count the number of elements in this list.
;;
;; - append : [List X] -> [List X]
;;   Append the elements of this list with the given list.
;;
;; - reverse : -> [List X]
;;   Reverse the elements of this list.
;;
;; - map [Y] : [X -> Y] -> [List Y]
;;   Apply given function to each element of this list and
;;   construct a list of the results
;;
;; - filter : [X -> Boolean] -> [List X]
;;   Select elements that satisfy the given predicate.
;;
;; - foldr [Y] : [X Y -> Y] Y -> Y
;;   Fold right over this list.
;;
;; - foldl [Y] : [X Y -> Y] Y -> Y
;;   Fold left over this list.

;; A [Cons X] implements [List X] and: 
;;
;; - first : -> X
;;   Get first element of this non-empty list.
;;
;; - rest : -> [List X]
;;   Get the reset of this non-empty list.

;; IMPLEMENTATION
;; --------------
;; A (new empty%) implements [List X]
;; A (new cons% X [Listof X]) implements [Cons X]

(define-class empty%
  (check-expect (send mt cons 0) (new cons% 0 mt))
  (define (cons x) (new cons% x this))

  (check-expect (send mt length) 0)
  (define (length) 0)

  (check-expect (send mt append ls) ls)
  (define (append ls) ls)

  (check-expect (send mt reverse) mt)
  (define (reverse) this)

  (check-expect (send mt reverse-acc ls) ls)
  (define (reverse-acc ls) ls)

  (check-expect (send mt map add1) mt)
  (define (map f) this)

  (check-expect (send mt foldr + 0) 0)
  (define (foldr f b) b)

  (check-expect (send mt foldl + 0) 0)
  (define (foldl f b) b))

(define-class cons%
  (fields first rest)

  (check-expect (send ls cons 0) (new cons% 0 ls))
  (define (cons x) (new cons% x this))

  (check-expect (send ls length) 3)
  (define (length) 
    (add1 (send (send this rest) length)))

  (check-expect (send ls append mt) ls)
  (define (append ls)
    (new cons% 
         (send this first)
         (send (send this rest) append ls)))

  (check-expect (send ls reverse)
                (new cons% 4 (new cons% 2 (new cons% 3 mt))))
  (define (reverse)
    (reverse-acc (new empty%)))

  (check-expect (send ls reverse-acc ls)
                (new cons% 4 (new cons% 2 (new cons% 3 ls))))
  ;; ACCUM: elements seen in reverse order.
  (define (reverse-acc ls)
    (send (send this rest) reverse-acc 
          (new cons% (send this first) ls)))

  (check-expect (send ls map add1)
                (new cons% 4 (new cons% 3 (new cons% 5 mt))))
  (define (map f)
    (new cons% (f (send this first))
         (send (send this rest) map f)))

  (check-expect (send ls foldr + 0) 9)
  (define (foldr f b)
    (f (this . first) (this . rest . foldr f b)))

  (check-expect (send ls foldl + 0) 9)
  (define (foldl f b)
    (this . rest . foldl f (f (this . first) b))))

(define mt (new empty%))
(define ls (new cons% 3 (new cons% 2 (new cons% 4 mt))))



}