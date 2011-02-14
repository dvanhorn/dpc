#lang class3
(require (only-in racket random begin void))
(require class1/universe)
(require 2htdp/image)

(define WIDTH       500)                             ; px
(define HEIGHT      500)                             ; px
(define MIN-Z       (make-rectangular 0 0))          ; px²
(define MAX-Z       (make-rectangular WIDTH HEIGHT)) ; px²
(define MAX-V       (/ MAX-Z 5))                     ; px²/s
(define TICK-RATE   (exact->inexact 1/20))           ; s

;
; complex
;

(define x-of real-part)
(define y-of imag-part)

;
; random
;

; random-between : Integer Integer -> Integer
; Randomly choose a number between a and b (inclusive)
(define (random-between a b)
  (+ a (random (+ 1 (- b a)))))

; choose : [Listof X] -> X
; Randomly choose an element from the list
(define (choose xs)
  (list-ref xs (random (length xs))))

; flip : Number -> Boolean
; Flip a biased coin
(define (flip p)
  (< (random) p))

; random-color : -> Color
; Randomly choose a color from a set of common colors
(define (random-color)
  (make-color (random-between 128 255)
              (random-between 128 255)
              (random-between 128 255)))

; random-complex-within : Complex -> Complex
; Randomly choose a complex within the rectangle bounded by z1 and z2
(define (random-complex-within z1 z2)
  (make-rectangular (random-between (x-of z1) (x-of z2))
                    (random-between (y-of z1) (y-of z2))))

;
; Ball
;

(define-class ball%
  (fields z v a radius)

  (define/public (speed)
    (magnitude (field v)))

  (define/public (draw)
    (circle (field radius) "solid" (this . color)))

  (define/public (distance-to that)
    (magnitude (- (this . z) (that . z))))

  (define/public (distance-within that)
    (max 0 (- (this . distance-to that)
              (field radius)
              (that . radius))))

  (define/public (overlaps? that)
    (not (positive? (this . distance-within that))))

  (define/public (contains? z)
    (<= (magnitude (- z (field z))) (field radius)))

  (define/public (contain)
    (cond
      [(or (this . invalid-left) (this . invalid-right))  (this . reflect-x)]
      [(or (this . invalid-top)  (this . invalid-bottom)) (this . reflect-y)]
      [else                                               this]))

  (define/public (invalid-left)
    (and (< (- (x-of (this . z)) (field radius)) (x-of MIN-Z))
                (< (x-of (this . v)) 0)))

  (define/public (invalid-right)
    (and (> (+ (x-of (this . z)) (field radius)) (x-of MAX-Z))
                (> (x-of (this . v)) 0)))

  (define/public (invalid-top)
    (and (< (- (y-of (this . z)) (field radius)) (y-of MIN-Z))
                (< (y-of (this . v)) 0)))

  (define/public (invalid-bottom)
    (and (> (+ (y-of (this . z)) (field radius)) (y-of MAX-Z))
                (> (y-of (this . v)) 0)))

  (define/public (reflect-x)
    (begin
      (set-field! v (conjugate (- (field v))))
      this))

  (define/public (reflect-y)
    (begin
      (set-field! v (conjugate (field v)))
      this))

  (define/public (line-from b i)
    (let* ([Δ (* 1/2 (- (field z) (b . z)))]
           [w (+ (b . z) Δ)])
      (scene+line i
                  (x-of (b . z))
                  (y-of (b . z))
                  (x-of w)
                  (y-of w)
                  (this . color))))

  )

;
; LiveBall
;

(define-class live-ball%
  (super ball%)
  (fields left right color)

  (constructor (z-)
    (fields 'none 'none
            (random-color)
            z- (random-v) 0
            (random-radius))
    (this . left!  this)
    (this . right! this))

  (define/public (left! b)
    (set-field! left b))

  (define/public (right! b)
    (set-field! right b))

  (define/public (links)
    (list (field left) (field right)))

  (define/public (tick)
    (if (this . should-die?)
      (dying-ball% (field z) (field v) (field a) (field radius))
      (begin
        (set-field! z (+ (field z) (* TICK-RATE (field v))))
        (set-field! v (+ (field v) (* TICK-RATE (field a))))
        (this . contain))))

  (define/public (friends? that)
    (equal? (this . color) (that . color)))

  (define/public (friend that)
    (if (and (that . friendly?)
             (not (this . friends? that)))
      (local [(define old-this-left  (this . left))
              (define old-that-right (that . right))]
        (begin
          (this . left!  that)
          (that . right! this)
          (send old-this-left  right! old-that-right) ; FIXME .
          (send old-that-right left!  old-this-left)
          (this . mimic that)))
      (void)))

  (define/public (mimic that)
    (if (not (this . friends? that))
      (begin
        (set-field! color (that . color))
        ((field right) . mimic that))
      (void)))

  (define/public (click z)
    (this . die))

  (define/public (die)
    (if (not (this . should-die?))
      (begin
        (set-field! color "black")
        ((field left) . die))
      (void)))

  (define/public (should-die?)
    (equal? "black" (this . color)))

  (define/public (friendly?)
    true)

  (define/public (dead?)
    false)

  )

;
; DyingBall
;

(define-class dying-ball%
  (super ball%)
  (fields life)

  (constructor (z- v- a- radius-)
    (fields 1 z- v- a- radius-))

  (define/public (color)
    (local [(define gray (round (* 255 (- 1 (field life)))))]
      (make-color gray gray gray)))

  (define/public (tick)
    (if (> (field life) .01)
      (begin
        (set-field! life (* .9 (field life)))
        this)
      (dead-ball%)))

  (define/public (click)
    (void))

  (define/public (links)
    empty)

  (define/public (friend that)
    (void))

  (define/public (friendly?)
    false)

  (define/public (dead?)
    false)

  )

;
; DeadBall
;

(define-class dead-ball%

  (define/public (dead?)
    true)

  )

; TODO Remove acceleration -----------------------------------------------------

(define (random-z)
  (random-complex-within MIN-Z MAX-Z))

(define (random-v)
  (random-complex-within (- MAX-V) MAX-V))

(define (random-radius)
  (random-between 10 20))

(define (random-ball-at z)
  (new live-ball% z))

(define (random-ball)
  (random-ball-at (random-z)))

;
; World
;

(define-class world%
  (fields balls)

  (define/public (tick-rate)
    TICK-RATE)

  (define/public (to-draw)
    (foldr (λ (b scn)
             (foldr (λ (a i) (a . line-from b i))
                    (place-image (b . draw)
                                 (x-of (b . z))
                                 (y-of (b . z))
                                 scn)
                    (b . links)))
           (empty-scene WIDTH HEIGHT)
           (field balls)))

  (define/public (on-tick)
    (begin
      (set-field! balls (filter (λ (b) (not (b . dead?)))
                                (map (λ (b) (b . tick))
                                     (field balls))))
      (map-pairs (λ (a b)
                   (if (a . overlaps? b)
                     (a . friend b)
                     (void)))
                 (pairs (field balls)))
      this))

  (define/public (on-mouse x y m)
    (cond
      [(mouse=? m "button-down")
       (begin
         (this . target-at (make-rectangular x y)
               . click (make-rectangular x y))
         this)]
      [else this]))

  (define/public (target-at z)
    (foldr (λ (b r) (if (b . contains? z) b r))
           this
           (field balls)))

  (define/public (click z)
    (this . spawn-at z))

  (define/public (on-key k)
    (cond
      [(key=? k "q") (stop-with this)]
      [(key=? k "n") (begin (this . spawn) this)]
      [else          this]))

  (define/public (spawn)
    (this . spawn-at (random-z)))

  (define/public (spawn-at z)
    (set-field! balls (cons (random-ball-at z) (field balls))))

  )

(define (tails xs)
  (if (empty? xs)
    (list empty)
    (cons xs (tails (rest xs)))))

(define (pairs xs)
  (apply append
         (map (λ (a bs)
                (map (λ (b) (list a b)) bs))
              xs
              (rest (tails xs)))))

(define (map-pairs f xys)
  (map (λ (xy) (f (first xy) (second xy))) xys))

(define (random-world)
  (new world% (repeat 5 random-ball)))

(define (repeat n f)
  (if (<= n 0)
      empty
      (cons (f) (repeat (- n 1) f))))

;
; go
;

(big-bang (random-world))
