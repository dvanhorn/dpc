#lang class4
(require class4/universe)
(require 2htdp/image)
(require (only-in racket when for-each printf set! void))
(require racket/set)

(provide (all-defined-out))

(define WIDTH       500)                              ; px
(define HEIGHT      500)                              ; px
(define LIGHT-SPEED 1000)                             ; px/s
(define MIN-LOC     0)
(define MAX-LOC     (make-rectangular WIDTH HEIGHT))
(define TICK-RATE   (exact->inexact 1/20))            ; s

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

; random-color : -> Color
; Randomly choose a color from a set of common colors
(define (random-color)
  (choose (list "red" "blue" "green" "yellow" "orange" "purple")))

; random-complex-within : Complex Complex -> Complex
; Randomly choose a complex within the rectangle bounded by z1 and z2
(define (random-complex-within z1 z2)
  (make-rectangular (random-between (real-part z1) (real-part z2))
                    (random-between (imag-part z1) (imag-part z2))))

;
; Geometry
;

(define (x-of z)
  (real-part z))

(define (y-of z)
  (imag-part z))

(define (norm z)
  (/ z (magnitude z)))

(define (mod-complex z w)
  (make-rectangular (mod-real (real-part z) (real-part w))
                    (mod-real (imag-part z) (imag-part w))))

(define (mod-real x y)
  (if (negative? x)
    (- y (mod-real (- x) y))
    (- x (* y (floor (/ x y))))))

(check-within (mod-real 8/10 5/10) 3/10 1/100)
(check-within (mod-real -8/10 5/10) 2/10 1/100)
(check-within (mod-real 18/10 5/10) 3/10 1/100)

(define (cap-complex z r)
  (if (zero? z)
    z
    (make-polar (min r (magnitude z)) (angle z))))

;
; Blob
;
(define-class base-blob%
  (fields loc vel acc area color)

  (define/public (x)
    (x-of (loc)))

  (define/public (y)
    (y-of (loc)))

  (define/public (radius)
    (sqrt (/ (area) pi)))

  (define/public (draw)
    (circle (radius) "solid" (color)))

  (define/public (tick!)
    (begin
      (set-field! loc (+ (loc) (* TICK-RATE (vel))))
      (set-field! vel (cap-complex (+ (vel) (* TICK-RATE (acc)))
                                   LIGHT-SPEED))
      (rebound!)))

  (define/public (rebound!)
    (let ([right (λ (z) (make-rectangular (+ (abs (x-of z))) (y-of z)))]
          [left  (λ (z) (make-rectangular (- (abs (x-of z))) (y-of z)))]
          [down  (λ (z) (make-rectangular (x-of z) (+ (abs (y-of z)))))]
          [up    (λ (z) (make-rectangular (x-of z) (- (abs (y-of z)))))])
      (begin
        (when (<= (- (x) (radius)) (x-of MIN-LOC))
          (set-field! vel (right (vel))))
        (when (<= (- (y) (radius)) (y-of MIN-LOC))
          (set-field! vel (down (vel))))
        (when (>= (+ (x) (radius)) (x-of MAX-LOC))
          (set-field! vel (left (vel))))
        (when (>= (+ (y) (radius)) (y-of MAX-LOC))
          (set-field! vel (up (vel))))
        this)))

  (define/public (overlap? that)
    (< (magnitude (- (loc) (that . loc)))
       (+ (radius) (that . radius))))

  ; To be overridden (noop)
  (define/public (collide! that) (void))

  )

;
; World
;

; DIRTY UGLY HACK to workaround constructor+super bug
(define down-keys (set))

(define-class base-world%

  ; DIRTY UGLY HACK
  ;(fields player enemies down-keys)
  (fields player enemies)

  ; DIRTY UGLY HACK
  ;(constructor (p es)
  ;  (fields p es (set)))
  (constructor (p es)
    (fields p es)
    (set! down-keys (set)))

  ; DIRTY UGLY HACK
  ;(define/public (get-down-keys)     (down-keys))
  ;(define/public (set-down-keys! ks) (set-field! down-keys ks))
  (define/public (get-down-keys)     down-keys)
  (define/public (set-down-keys! ks) (set! down-keys ks))

  (define/public (blobs)
    (cons (player) (enemies)))

  (define/public (tick-rate)
    TICK-RATE)

  (define/public (to-draw)
    (foldl (λ (b scn)
             (place-image (b . draw) (b . x) (b . y) scn))
           (empty-scene WIDTH HEIGHT)
           (blobs)))

  (define/public (on-tick)
    (begin
      (for-each  (λ (b) (b . tick!)) (blobs))
      (for-pairs (λ (a b)
                   (when (a . overlap? b)
                     (a . collide! b)))
                 (blobs))
      this))

  (define/public (on-key k)
    (if (not (set-member? (get-down-keys) k))
      (begin
        (set-down-keys! (set-add (get-down-keys) k))
        (this . on-key-change (set->list (get-down-keys))))
      this))

  (define/public (on-release k)
    (if (set-member? (get-down-keys) k)
      (begin
        (set-down-keys! (set-remove (get-down-keys) k))
        (this . on-key-change (set->list (get-down-keys))))
      this))

  ; To be overridden (noop)
  (define/public (on-key-change ks) this)

  )

(define (key-in? k ks)
  (ormap (λ (l) (key=? l k)) ks))

(define (set->list s)
  (set-map s (λ (x) x)))

(define (rests xs)
  (if (empty? xs)
    (list '())
    (cons xs (rests (rest xs)))))

(check-expect (rests '())      '(()))
(check-expect (rests '(1 2 3)) '((1 2 3) (2 3) (3) ()))

(define (for-pairs f xs)
  (for-each (λ (xs0)
              (when (not (empty? xs0))
                (let ([x (first xs0)]
                      [ys (rest xs0)])
                  (for-each (λ (y) (f x y)) ys))))
    (rests xs)))
