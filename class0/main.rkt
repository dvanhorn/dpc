#lang racket/base
(require "define-class.rkt"
         (except-in lang/htdp-intermediate-lambda define require #%module-begin
                    define-struct image? 
                    check-expect check-within check-error check-range check-member-of)
         "../class1/test-engine/racket-tests.rkt")

(require (prefix-in isl+: lang/htdp-intermediate-lambda))
(require (prefix-in r: racket))

(provide (all-from-out "define-class.rkt")
         (all-from-out lang/htdp-intermediate-lambda)
         define test require provide
         all-defined-out only-in all-from-out except-in
         (except-out (all-from-out "../class1/test-engine/racket-tests.rkt")
                     test))

(require (for-syntax syntax/parse racket/base))

(define-syntax (#%module-begin stx)
  (syntax-parse stx 
    [(_ forms ...)
     #'(r:#%module-begin (reset-tests) forms ... (test) (reset-tests))]))

(provide #%module-begin)

(provide define-struct)
(require (for-syntax syntax/struct))
(define-syntax (define-struct stx)
  (syntax-parse stx
    [(_ name:id (field:id ...))
     (with-syntax ([(_ _ _ acc ...) 
                    (build-struct-names #'name (syntax->list #'(field ...)) #f #t)])
       #'(r:define-struct name (field ...) #:transparent 
			  #:property prop:custom-print-quotable 'never
                          #:property prop:custom-write
                          (r:λ (s p w?) 
                            (fprintf p "(make-~a" 'name)
                            (fprintf p " ~v" (acc s))
                            ...
                            (fprintf p ")"))))]))
                  
    
