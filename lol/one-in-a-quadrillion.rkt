#lang racket
;; The One-in-a-Quadrillion Game!
;; In this game, you pick a number from 1 to 1000000000000000, 
;; and the computer has to guess it.

;; Based on The Guess-My-Number Game, p. 50 of Barski's Land of Lisp.
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)

(define *MAX* 1000000000000000) ; quadrillion 10^15

;; A Range is a (range lo hi)
(struct range (lo hi) #:transparent) ;; Interp: [lo,hi]

;; Range -> Nat
(define (guess-my-number r)
  (arithmetic-shift (+ (range-lo r) (range-hi r)) -1))

(check-expect (guess-my-number (range 0 100)) 50)

;; Range -> Range
(define (bigger r)
  (range (add1 (guess-my-number r))
         (range-hi r)))

(check-expect (bigger (range 0 100)) (range 51 100))

;; Range -> Range
(define (smaller r)
  (range (range-lo r)
         (sub1 (guess-my-number r))))

(check-expect (smaller (range 0 100)) (range 0 49))

;; Range -> Range
(define (start-over r)
  (range 0 *MAX*))

(check-expect (start-over (range 51 100)) (range 0 *MAX*))

(test)

(big-bang (range 0 *MAX*)
 (on-draw 
  (λ (r) 
    (text (number->string (guess-my-number r))
          150
          "purple")))
 (on-key 
  (λ (r k) 
    (cond [(key=? "up" k) (bigger r)]
          [(key=? "down" k) (smaller r)]
          [(key=? "s" k) (start-over r)]
          [else r]))))
