#lang racket
(require syntax/parse)
(provide (all-defined-out))

(define-struct class-name (id)
  #:property prop:procedure 
  (λ (inst stx) 
    (raise-syntax-error #f 
                        "class names may not be used as expressions" 
                        stx)))

(define-syntax-class cls-name
  #:description "a class name"
  #:opaque
  (pattern name:id
           #:fail-unless 
           (class-name? (syntax-local-value #'name (λ () #f))) 
           "expected a class name"
           #:with real-name 
           (class-name-id (syntax-local-value #'name (λ () #f)))))

