#lang racket
(provide (all-defined-out))
(require test-engine/scheme-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shared data definitions

;; A Zombie is a Meat.
;; A Player is a Meat.
;; A Meat is one of:         ; interp:
;; - (+ (- Nat) (* +i Nat))  ; dead meat
;; - (+ (+ Nat) (* +i Nat))  ; live meat
;; A Posn is a (+ Nat (* +i Nat)).

;; (struct dead (posn))
(define dead? (compose negative? real-part))
(define dead (compose sub1 -))
(define dead-pos (compose - add1))

(check-expect (dead-pos (dead 0)) 0)
(check-expect (dead? 0) false)
(check-expect (dead? (dead 0)) true)

;; Meat -> Posn
(define (meat-pos m)
  (cond [(dead? m) (dead-pos m)]
        [else m]))

(check-expect (meat-pos 0) 0)
(check-expect (meat-pos (dead 0)) 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shared constants

(define *dim* 400+400i)
(define *cell-size* 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages

(define (teleport p) (list 'teleport p))
(define (toward p) (list 'toward p))
(define (toward? sexp) ((starts-with 'toward) sexp))
(define (teleport? sexp) ((starts-with 'teleport) sexp))
(define toward-posn second)
(define teleport-posn second)
(define (starts-with s)
  (λ (x)
    (and (cons? x)
         (eq? s (first x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Posn library

;; Nat Nat -> Posn
(define (pos x y)
  (+ x (* 0+1i y)))
(define pos-x real-part)
(define pos-y imag-part)
(define (pos-abs p) 
  (pos (abs (real-part p))
       (abs (imag-part p))))
(define (pos-sum p)
  (+ (pos-x p) (pos-y p)))
(define (random-posn p)
  (let ((r (random (* (real-part p) (imag-part p)))))
    (pos (quotient r (pos-y p))
          (remainder r (pos-y p)))))

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

(test)
