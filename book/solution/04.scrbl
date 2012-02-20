#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct check-expect))
          (for-label (except-in class/1 define-struct check-expect)))

@title[#:tag "soln04"]{Solution: Functional programming with objects}

This is a solution for the
@secref["Functional_programming_with_objects"] exercise.

@codeblock{
#lang class/1

;; A [IFun X Y] implements:
;; - apply : X -> Y
;;   Apply this function to the given input.
;; - compose : [IFun Y Z] -> [IFun X Z]
;;   Produce a function that applies this function to its input,
;;   then applies the given function to that result.

;; A (new fun% [X -> Y]) implements [IFun X Y].
(define-class fun%
  (fields f)

  (define (apply x)
    ((this . f) x))

  (define (compose g)
    (new fun% (λ (x)
                (g . apply (this . apply x))))))

(define addone (new fun% add1))
(define subone (new fun% sub1))

(check-expect ((addone . compose subone) . apply 5) 5)
(check-expect ((addone . compose addone) . apply 5) 7)
}