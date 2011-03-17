#lang class4


(define-class foo%
  (fields x y)
  (define/public (m i) i))

(define-class bar%
  (super foo%)
  (fields z)
  (define/public (m i) (string-append i (super i))))

(define c (foo% 1 2))

(define d (bar% 1 2 3))

(instanceof c foo%)
(instanceof c bar%)
(instanceof d foo%)
(instanceof d bar%)

(send d m "foo")
(send c m "foo")