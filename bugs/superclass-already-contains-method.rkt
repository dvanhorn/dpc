#lang class1

(define-class a%
  (fields one))

(define-class b%
  (super a%)
  (fields one))

; Error msg: class*: superclass #<class:a%> already contains method: one for class: b%

; Desired error msg: class*: superclass #<class:a%> already contains field: one for class: b%