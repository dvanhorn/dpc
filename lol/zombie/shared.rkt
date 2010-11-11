;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname shared) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ISL+λ
(require class0)
(provide (all-defined-out))
(require test-engine/scheme-tests)
;Doesn't look like we get the racket's require here.
;(require (only-in racket for/list for/fold for/set))
;Hack work-around:
(require "set.rkt")
(provide (all-from-out "set.rkt"))

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

(check-expect (teleport 5+5i) (list 'teleport 5+5i))
(check-expect (toward 5+5i) (list 'toward 5+5i))
(check-expect (teleport? (teleport 5+5i)) true)
(check-expect (teleport? (toward 5+5i)) false)
(check-expect (toward? (toward 5+5i)) true)
(check-expect (toward? (teleport 5+5i)) false)

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

(check-expect (pos 5 5) 5+5i)
(check-expect (pos-x (pos 3 4)) 3)
(check-expect (pos-y (pos 3 4)) 4)
(check-expect (pos-abs (pos -3 -4)) (pos 3 4))
(check-expect (pos-sum (pos 3 4)) 7)
(check-expect (< -1 (pos-x (random-posn 500+200i)) 500)
              true)
(check-expect (< -1 (pos-y (random-posn 500+200i)) 200)
              true)




