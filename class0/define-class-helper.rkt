#lang racket

(require syntax/parse)

(define-struct class-name (id)
  #:property prop:procedure (λ (inst stx) (raise-syntax-error #f "class names may not be used as expressions" stx)))
#;
(define-struct interface-name (id)
  #:property prop:procedure (λ (inst stx) (raise-syntax-error #f "interface names may not be used as expressions" stx)))

(define-syntax-class cls-name
  #:description "a class name"
  #:opaque
  (pattern name:id
           #:fail-unless (class-name? (syntax-local-value #'name (λ () #f))) "expected a class name"
           #:with real-name (class-name-id (syntax-local-value #'name (λ () #f)))))
#;
(define-syntax-class ifc-name
  #:description "an interface name"
  #:opaque
  (pattern name:id
           #:fail-unless (interface-name? (syntax-local-value #'name (λ () #f))) "expected an interface name"
           #:with real-name (interface-name-id (syntax-local-value #'name (λ () #f)))))
(provide (all-defined-out))