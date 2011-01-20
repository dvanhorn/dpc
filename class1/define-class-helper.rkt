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
           #:fail-unless 
           (interface-name? (syntax-local-value #'name (λ () #f))) 
           "expected an interface name"
           #:with real-name 
           (interface-name-id (syntax-local-value #'name (λ () #f)))))

