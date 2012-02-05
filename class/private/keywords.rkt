#lang racket

(define-syntax-rule (def-kw id ...)
  (begin 
    (define-syntax (id stx) 
      (raise-syntax-error #f "can only be used inside define-class" stx))
    ...
    (provide id ...)))

(def-kw super implements field fields constructor)