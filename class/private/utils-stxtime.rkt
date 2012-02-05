#lang racket/base
(provide (all-defined-out))
(require syntax/parse
         (for-template 
          racket/base
          (prefix-in r: racket)
          (prefix-in isl+: (only-in lang/htdp-intermediate-lambda define))
          (prefix-in isl+: (only-in "../test-engine/racket-tests.rkt"
                                    check-expect check-within 
                                    check-error check-member-of
                                    check-range)) 
          "../test-engine/racket-tests.rkt"
          "keywords.rkt"))


(define reserved-words
  '(field define super define/public define/private fields
          constructor))

(define-syntax-class (member-name names)    
  (pattern name:id
           #:fail-when 
           (memq (syntax-e #'name) reserved-words)             
           (format "~a is not allowed as the name of a method or field"
                   (syntax-e #'name))
           #:fail-when 
           (memf (λ (id) (eq? (syntax-e id) (syntax-e #'name))) names)
           (format "~a is already defined as a field, and can't be redefined"
                   (syntax-e #'name))))

(define-syntax-class (member-def names)
  #:description "a method definition"
  #:opaque #:commit
  #:literals (define)    
  (pattern ((~or (~literal r:define/public) (~literal r:define/private)) 
            ((~var f (member-name names)) x:id ...) e:expr)
           #:with def this-syntax)
  (pattern ((~or (~datum define/public) (~datum define/private)) 
            ((~var f (member-name names)) x:id ...) e:expr)
           #:with def this-syntax
           #:fail-when #t
           "define/public is not supported, use define instead")
  (pattern (define ((~var f (member-name names)) x:id ...) e:expr)
           #:with def #'(r:define/public (f x ...) e)))

(define-splicing-syntax-class fields-spec
  #:attributes ([fld 1])
  #:commit #:description "a fields specification"
  [pattern (~optional ((~literal fields) ~! (~var fld (member-name null)) ...)
                      #:defaults ([(fld 1) null]))])

(define-syntax-class test
  #:literals (isl+:check-expect 
              isl+:check-within
              isl+:check-error
              isl+:check-member-of
              isl+:check-range)
  #:description "a test case"
  #:opaque
  #:commit
  [pattern (~or (isl+:check-expect <act> <exp>)
                (isl+:check-within <act> <exp> <rng>)
                (isl+:check-error <act> <msg>)
                (isl+:check-error <act>)
                (isl+:check-member-of <act> <exp0> <exp1> ...)
                (isl+:check-range <act> <lo> <hi>))])



(define-struct interface-name (id)
  #:property prop:procedure 
  (λ (inst stx) 
    (raise-syntax-error #f 
                        "interface names may not be used as expressions" 
                        stx)))

(define-syntax-class ifc-name
  #:description "an interface name"
  #:opaque
  (pattern name:id
           #:attr v (syntax-local-value #'name (λ () #f))
           #:fail-unless 
           (interface-name? (attribute v))
           "expected an interface name"
           #:with real-name (interface-name-id (attribute v))
           
           ;#:attr methods (interface-name-methods (attribute v))
           ))


(define-struct class-name (id flds methods)
  #:property prop:procedure 
  (λ (inst stx) 
    (raise-syntax-error #f 
                        "class names may not be used as expressions" 
                        stx)))

(define-syntax-class cls-name
  #:description "a class name"
  #:opaque
  (pattern name:id
           #:attr v (syntax-local-value #'name (λ () #f))
           #:fail-unless 
           (class-name? (attribute v)) 
           "expected a class name"
           #:attr fields (class-name-flds (attribute v))
           #:attr methods (map syntax-local-introduce (class-name-methods (attribute v)))
           #:with real-name (class-name-id (attribute v))))