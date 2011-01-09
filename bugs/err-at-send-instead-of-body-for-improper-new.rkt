#lang racket

(require class0)

(define-class A%

  (define/public (ok)
    (send 0 oops))

  (define/public (bad)
    (new A% 0)))

; Run each of these separately in DrRacket:
;(send (new A%) ok)  ; Good error location: body of `ok' method
;(send (new A%) bad) ; Bad error location: call site of `bad' method
