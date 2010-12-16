#lang racket/base
(require "define-class.rkt"
         (except-in lang/htdp-intermediate-lambda define require #%module-begin)
         test-engine/racket-tests)

(require (prefix-in isl+: lang/htdp-intermediate-lambda))
(require (prefix-in r: racket))

(provide (all-from-out "define-class.rkt")
         (all-from-out lang/htdp-intermediate-lambda)
         define test require provide
         all-defined-out only-in all-from-out except-in
         (except-out (all-from-out test-engine/racket-tests) test))

(require (for-syntax syntax/parse racket/base))

(define-syntax (#%module-begin stx)
  (syntax-parse stx 
    [(_ forms ...)
     #'(r:#%module-begin forms ... (test))]))

(provide #%module-begin)
    