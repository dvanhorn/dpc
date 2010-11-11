#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set library

(define (set-map-list f s)
  (for/list ([x (in-set s)]) (f x)))
(define (set-map f s)
  (for/set ([x (in-set s)]) (f x)))
(define (set-ormap f s)
  (for/or ([x (in-set s)]) (f x)))
(define (set-fold f b s)
  (for/fold ([r b])
    ([x (in-set s)])
    (f x r)))
(define (set-filter f s)
  (for/set ([x (in-set s)]
            #:when (f x))
           x))
