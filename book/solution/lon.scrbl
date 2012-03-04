#lang scribble/manual
@(require class/utils
          (for-label (except-in class/0 check-expect length
                                append reverse map first rest max))
          (for-label (only-in lang/htdp-intermediate-lambda check-expect)))

@title[#:tag "Lists_of_Numbers_solution"]{Solution:
@secref{Lists_of_Numbers}}

This is a solution for the @secref{Lists_of_Numbers} exercise.

@codeblock{
#lang class/0
;; ==========================================================
;; Lists of Numbers

;; INTERFACE
;; ---------
;; A ListofNumber implements:
;;
;; - length : -> Natural
;;   Count the number of elements in this list.
;;
;; - append : ListofNumber -> ListofNumber
;;   Append the elements of this list with the given list.
;;
;; - sum : -> Number
;;   Add all the elements of this list.
;;
;; - prod : -> Number
;;   Multiply all the elements of this list.
;;
;; - contains? : Number -> Boolean
;;   Is the given number an element in this list?
;;
;; - reverse : -> ListofNumber
;;   Reverse the elements of this list.
;;
;; - map : [Number -> Number] -> ListofNumber
;;   Apply given function to each element of this list and
;;   construct a list of the results
;;
;; - max-acc : Number -> Number
;;   Find largest number between given and this list of numbers. 
;; 
;; A NeListofNumber implements ListofNumber and:
;;
;; - max : -> Number
;;   Find largest number in this non-empty list.

;; IMPLEMENTATION
;; --------------
;; A (new empty%) implements ListofNumber
;; A (new cons% Number ListofNumber) implements NeListofNumber

(define-class empty%
  (check-expect (send mt length) 0)
  (define (length) 0)

  (check-expect (send mt append ls) ls)
  (define (append ls) ls)

  (check-expect (send mt sum) 0)
  (define (sum) 0)
  
  (check-expect (send mt prod) 1)
  (define (prod) 1)

  (check-expect (send mt contains? 5) false)
  (define (contains? n) false)

  (check-expect (send mt reverse) mt)
  (define (reverse) this)

  (check-expect (send mt reverse-acc ls) ls)
  (define (reverse-acc ls) ls)

  (check-expect (send mt map add1) mt)
  (define (map f) this)

  (check-expect (send mt max-acc 5) 5)
  (define (max-acc a) a))

(define-class cons%
  (fields first rest)

  (check-expect (send ls length) 3)
  (define (length) 
    (add1 (send (send this rest) length)))

  (check-expect (send ls append mt) ls)
  (define (append ls)
    (new cons% 
         (send this first)
         (send (send this rest) append ls)))

  (check-expect (send ls sum) 9)
  (define (sum)
    (+ (send this first)
       (send (send this rest) sum)))

  (check-expect (send ls prod) 24)
  (define (prod)
    (* (send this first)
       (send (send this rest) prod)))

  (check-expect (send ls contains? 5) false)
  (check-expect (send ls contains? 2) true)
  (define (contains? n)
    (or (= n (send this first))
        (send (send this rest) contains? n)))

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


  (check-expect (send ls max-acc 8) 8)
  (check-expect (send ls max-acc 0) 4)
  ;; ACCUM: largest number seen so far.
  (define (max-acc a)
    (send (send this rest) max-acc 
          (cond [(> (send this first) a)
                 (send this first)]
                [else a])))

  (check-expect (send ls max) 4)
  (define (max)
    (send (send this rest) max-acc (send this first))))

(define mt (new empty%))
(define ls (new cons% 3 (new cons% 2 (new cons% 4 mt))))



}