#lang class1

(define x 0.1)
; define: bad syntax (multiple expressions after identifier) in:
; (define DT 0 |.| 1)

(define y ((λ (x) x) 0.1))
; .: not legal outside of method send syntax in: |.|
