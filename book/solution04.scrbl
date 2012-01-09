#lang scribble/manual
@(require #;"utils.rkt"
	  "../class/utils.rkt"
          (for-label (only-in lang/htdp-intermediate-lambda define-struct))
          (for-label (except-in class/1 define-struct)))

@title[#:tag "soln04"]{Functions}

This is a solution for the @seclink["assign_function"]{Function} exercise.

@#reader scribble/comment-reader
(racketmod
class/1
;; Part 1
(define-class fun%
  (fields f)
  (define/public (apply x)
    ((field f) x)))

;; Part 2
(define addone (fun% add1))
(define subone (fun% sub1))

;; Part 3
(define-class fun%
  (fields f)
  (define/public (compose g)
    (fun% (λ (x)
            (g #,dot apply (apply x))))))
)