#lang racket/base
(require "define-class.rkt"
         (except-in lang/htdp-intermediate-lambda define require #%module-begin
                    define-struct image? quote #%app 
                    check-expect check-within check-error check-range check-member-of)
         "../class1/test-engine/racket-tests.rkt")


(require (only-in "../class0/main.rkt" define-struct #%module-begin)
         (for-syntax racket/base syntax/parse))
(require (prefix-in isl+: lang/htdp-intermediate-lambda))
(require (prefix-in r: racket))

(provide (all-from-out "define-class.rkt")
         (all-from-out lang/htdp-intermediate-lambda)
	 quote
         #%module-begin |.| (rename-out [my-app #%app])
         define test require provide define-struct begin
         all-defined-out only-in all-from-out except-in
         (except-out (all-from-out "../class1/test-engine/racket-tests.rkt")
                     test))

(define-syntax (my-app stx)
  (syntax-parse stx #:literals (|.|)
    [(_ rcvr:expr |.| meth:id (~and (~not |.|) args:expr) ...)
     #'(send rcvr meth args ...)]
    [(_ rcvr:expr |.| meth:id (~and (~not |.|) args:expr) ... rest:expr ...)
     #'(my-app (send rcvr meth args ...) rest ...)]
    [(_ e ...)
     #'(#%app e ...)]))
      
(define-syntax (|.| stx)
  (raise-syntax-error #f "not legal outside of method send syntax" stx))


                  
   