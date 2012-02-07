#lang racket/base

(provide (all-defined-out)
         (all-from-out "keywords.rkt"))
(require 
 (prefix-in isl+: (only-in lang/htdp-intermediate-lambda define))
 (prefix-in isl+: (only-in "../test-engine/racket-tests.rkt"
                                   check-expect check-within 
                                   check-error check-member-of
                                   check-range)) 
 (prefix-in r: (combine-in racket/base racket/class))
 "../test-engine/racket-tests.rkt"
 "keywords.rkt"
 (for-syntax syntax/parse racket/base "utils-stxtime.rkt"))


(define-syntax object% (class-name #'r:object% null null))

;; for field construction
(define-struct field-wrapper (vals))

(define (wrap . x) (make-field-wrapper x))


(define-syntax (define-interface stx)
  (syntax-parse stx #:literals (super)
    [(_ name:id (super super-ifc:ifc-name) ... (meths:id ...))
     #:with -ifc (datum->syntax #f (syntax-e #'name))
     #'(begin (define-syntax name (interface-name #'-ifc))
              (define -ifc (r:interface (super-ifc.real-name ...) meths ...)))]))


(define-syntax (new stx)
  (syntax-parse stx
    [(_ cls:cls-name . args)
     #'(r:make-object cls.real-name . args)]))

(define-syntax (my-app stx)
  (syntax-parse stx #:literals (|.|)
    [(_ rcvr:expr (~and dot |.|) meth:id (~and (~not |.|) args:expr) ...)
     (syntax-property #'(send rcvr meth args ...)
                      'disappeared-use #'dot)]
    [(_ rcvr:expr (~and dot |.|) meth:id (~and (~not |.|) args:expr) ... rest:expr ...)
     (syntax-property #'(my-app (send rcvr meth args ...) rest ...)
                      'disappeared-use #'dot)]
    [(_ e ...)
     #'(#%app e ...)]))
      
(define-syntax (|.| stx)
  (raise-syntax-error #f "not legal outside of method send syntax" stx))

(define-syntax (-module-begin stx)
  (syntax-parse stx 
    [(_ forms ...)
     #'(r:#%module-begin (reset-tests) forms ... (test) (reset-tests))]))


(require (for-syntax syntax/struct))
(define-syntax (-define-struct stx)
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


(define-syntax (instanceof stx)
  (syntax-parse stx
    [(_ e:expr cls:cls-name)
     #'(r:is-a? e cls.real-name)]))