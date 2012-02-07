#lang racket
(require syntax/parse)
(provide (all-defined-out))

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

