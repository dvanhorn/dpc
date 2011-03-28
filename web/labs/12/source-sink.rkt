#lang class5
(require class5/universe)
(require 2htdp/image)
(require (only-in racket random when void))

;
; Random
;

; random-in-range : Real Real -> Real
; Randomly choose a real in the range (x,y)
(define (random-in-range x y)
  (+ x (* (random) (- y x))))

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
  (make-rectangular (random-in-range (real-part z1) (real-part z2))
                    (random-in-range (imag-part z1) (imag-part z2))))

;
; Geometry
;

; x-of : Complex -> Real
; The x component
(define (x-of z)
  (real-part z))

; y-of : Complex -> Real
; The y component
(define (y-of z)
  (imag-part z))

; norm : Complex -> Complex
; The unit vector (i.e. length 1) in the same direction as z
(define (norm z)
  (/ z (magnitude z)))

; cap-complex : Complex Real -> Complex
; Cap the length of z by r
(define (cap-complex z r)
  (if (zero? z)
    z
    (make-polar (min r (magnitude z)) (angle z))))

;
; Parameters
;

(define WIDTH       500)                              ; px
(define HEIGHT      500)                              ; px
(define LIGHT-SPEED 1000)                             ; px/s
(define MIN-LOC     0)
(define MAX-LOC     (make-rectangular WIDTH HEIGHT))
(define TICK-RATE   (exact->inexact 1/20))            ; s

; A  Location     is a Complex
; A  Velocity     is a Complex
; An Acceleration is a Complex
; A  Mass         is a positive Real
; A  Color        is a String

; A Point implements
;
; loc : -> Location
; The location
;
; vel : -> Velocity
; The velocity
;
; acc : -> Acceleration
; The acceleration
;
; x : -> Real
; The x coordinate of the location
;
; y : -> Real
; The y coordinate of the location
;
; tick! : ->
; Tick this Point

(define-interface none<%>  [])

(define (point-mixin %)
  (class (super % none<%>)
    (fields loc vel acc)

    (define/public (x)
      (x-of (loc)))

    (define/public (y)
      (y-of (loc)))

    (define/public (tick!)
      (begin
        (set-field! loc (+ (loc) (* TICK-RATE (vel))))
        (set-field! vel (cap-complex (+ (vel) (* TICK-RATE (acc)))
                                     LIGHT-SPEED))
        (rebound!)))

    ; ->
    ; Rebound our velocity if we're not within the walls
    (define/public (rebound!)
      (let ([right (λ (z) (make-rectangular (+ (abs (x-of z))) (y-of z)))]
            [left  (λ (z) (make-rectangular (- (abs (x-of z))) (y-of z)))]
            [down  (λ (z) (make-rectangular (x-of z) (+ (abs (y-of z)))))]
            [up    (λ (z) (make-rectangular (x-of z) (- (abs (y-of z)))))])
        (begin
          (when (<= (x) (x-of MIN-LOC))
            (set-field! vel (right (vel))))
          (when (<= (y) (y-of MIN-LOC))
            (set-field! vel (down (vel))))
          (when (>= (x) (x-of MAX-LOC))
            (set-field! vel (left (vel))))
          (when (>= (y) (y-of MAX-LOC))
            (set-field! vel (up (vel))))
          this)))))

; A Source is a (source% Location Velocity Acceleration)
; and implements Point
(define source%
  ((compose point-mixin)
   (class

     ; -> Real
     ; The radius
     (define/public (radius)
       30)

     ; -> Color
     ; The color
     (define/public (color)
       "blue")

     ; -> Image
     ; Draw us as a circle
     (define/public (draw)
       (circle (radius) "solid" (color))))))

; A Sink is a (sink% Mass Location Velocity Acceleration)
; and implements Point
(define sink%
  ((compose point-mixin)
   (class
     (fields mass)

     ; -> Real
     ; Our radius
     (define/public (radius)
       (sqrt (/ (this . mass) 2 pi)))

     ; -> Color
     ; Our color
     (define/public (color)
       "red")

     ; -> Image
     ; Draw us as a cirlce
     (define/public (draw)
       (circle (radius) "solid" (color))))))

; A Projectile is a (projectile% Location Velocity Acceleration)
; and implements Point
(define projectile%
  ((compose point-mixin)
   (class
     (fields mass)

     ; -> Image
     ; Draw us as a triangle
     (define/public (draw)
       (triangle 30 "solid" "black")))))

;
; World
;

; A World is a (world% Source [Listof Projectile] Sink)
(define world%
  (class
    (fields source projectiles sink)

    ; -> Number
    ; The tick rate of the World
    (define/public (tick-rate)
      TICK-RATE)

    ; -> Image
    ; Draw the World
    (define/public (to-draw)
      (foldl (λ (o scn)
               (place-image (o . draw) (o . x) (o . y) scn))
             (empty-scene WIDTH HEIGHT)
             (list* (source) (sink) (projectiles))))

    ; ->
    ; Tick the World
    (define/public (on-tick)
      (begin
        (map (λ (o) (o . tick!))
             (list* (source) (sink) (projectiles)))
        this))))

;
; More random
;

; random-loc : -> Location
; A random location within bounds
(define (random-loc)
  (random-complex-within 0 MAX-LOC))

; random-vel : -> Velocity
; A random velocity
(define (random-vel)
  (make-polar (random-in-range 20 50)
              (random-in-range 0 (* 2 pi))))

; random-vel-fast : -> Velocity
; A random fast velocity
(define (random-vel-fast)
  (* 5 (random-vel)))

; random-source : -> Source
; A randomly generated Source
(define (random-source)
  (source%
    (random-loc)      ; loc
    (random-vel)      ; vel
    0))               ; acc

; random-sink : -> Sink
; A randomly generated Sink
(define (random-sink)
  (sink%
    (random-loc)      ; loc
    (random-vel)      ; vel
    0                 ; acc
    1000))            ; mass

; random-projectile-at : Location -> Projectile
; A randomly generated Projectile at the given Location
(define (random-projectile-at loc)
  (projectile%
    loc               ; loc
    (random-vel-fast) ; vel
    0                 ; acc
    200))             ; mass

; random-world : -> World
; A randomly generated World
(define (random-world)
  (world% (random-source)
          (list (random-projectile-at (random-loc))
                (random-projectile-at (random-loc))
                (random-projectile-at (random-loc)))
          (random-sink)))

;
; big-bang
;

(big-bang (random-world))
