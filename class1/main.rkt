#lang racket/base
(require "define-class.rkt"
         (except-in lang/htdp-intermediate-lambda define require #%module-begin
                    define-struct image? quote)
         test-engine/racket-tests)

(require (only-in "../class0/main.rkt" define-struct #%module-begin))
(require (prefix-in isl+: lang/htdp-intermediate-lambda))
(require (prefix-in r: racket))

(provide (all-from-out "define-class.rkt")
         (all-from-out lang/htdp-intermediate-lambda)
	 quote
         #%module-begin
         define test require provide define-struct
         all-defined-out only-in all-from-out except-in
         (except-out (all-from-out test-engine/racket-tests) test))


                  
    
