#lang scribble/manual
@(require class/utils
          (for-label (except-in class/0 check-expect length
                                cons append reverse map filter foldl foldr first rest))
          (for-label (only-in lang/htdp-intermediate-lambda check-expect)))

@title[#:tag "Abstract_Lists_solution"]{Solution:
@secref{Abstract_Lists}}

This is a solution for the @secref{Abstract_Lists} exercise.

@codeblock{
#lang class/1
;; ==========================================================
;; Lists of X with inheritance

;; IMPLEMENTATION
;; --------------
;; A (new empty%) implements [List X], extends list%
;; A (new cons% X [Listof X]) implements [Cons X], extends list%

(define-class list%
  (check-expect (send mt empty) mt)
  (check-expect (send ls empty) mt)
  (define (empty) (new empty%))

  (check-expect (send mt cons 0) (new cons% 0 mt))
  (define (cons x) (new cons% x this))

  (check-expect (send mt length) 0)
  (check-expect (send ls length) 3)
  (define (length)
    (send this foldl (λ (x len) (add1 len)) 0))

  (check-expect (send mt append ls) ls)
  (check-expect (send ls append mt) ls)
  (define (append ls)
    (send this foldr (λ (x ls) (ls . cons x)) ls))

  (check-expect (send mt reverse) mt)
  (check-expect (send ls reverse)
                (new cons% 4 (new cons% 2 (new cons% 3 mt))))
  (define (reverse)
    (send this foldl (λ (x ls) (ls . cons x)) (this . empty)))

  (check-expect (send mt map add1) mt)
  (check-expect (send ls map add1)
                (new cons% 4 (new cons% 3 (new cons% 5 mt))))
  (define (map f)
    (send this foldr (λ (x ys) (ys . cons (f x))) (this . empty))))

(define-class empty%
  (super list%)

  (check-expect (send mt foldr + 0) 0)
  (define (foldr f b) b)

  (check-expect (send mt foldl + 0) 0)
  (define (foldl f b) b))

(define-class cons%
  (super list%)
  (fields first rest)

  (check-expect (send ls foldr + 0) 9)
  (define (foldr f b)
    (f (this . first) (this . rest . foldr f b)))

  (check-expect (send ls foldl + 0) 9)
  (define (foldl f b)
    (this . rest . foldl f (f (this . first) b))))

(define mt (new empty%))
(define ls (new cons% 3 (new cons% 2 (new cons% 4 mt))))



}