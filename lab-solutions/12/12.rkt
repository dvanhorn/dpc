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

(define WIDTH       680)                              ; px
(define HEIGHT      730)                              ; px
(define LIGHT-SPEED 1000)                             ; px/s
(define MIN-LOC     0)
(define MAX-LOC     (make-rectangular WIDTH HEIGHT))
(define TICK-RATE   (exact->inexact 1/20))            ; s

;
; Mixins
;

(define-interface none<%> [])

(define (point-mixin %)
  (class (super % none<%>)
    (fields loc vel acc)

    (define/public (x)
      (x-of (loc)))

    (define/public (y)
      (y-of (loc)))

    (define/public (acc! a)
      (set-field! acc a))

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
          (when (<= (x) (x-of MIN-LOC))
            (set-field! vel (right (vel))))
          (when (<= (y) (y-of MIN-LOC))
            (set-field! vel (down (vel))))
          (when (>= (x) (x-of MAX-LOC))
            (set-field! vel (left (vel))))
          (when (>= (y) (y-of MAX-LOC))
            (set-field! vel (up (vel))))
          this)))

    ))

(define-interface tick<%> [tick!])

(define (time-mixin %)
  (class (super % tick<%>)
    (fields time)

    (define/public (tick!)
      (begin
        (super)
        (set-field! time (+ TICK-RATE (time)))))

    ))

(define (mass-mixin %)
  (class (super % none<%>)
    (fields mass)

    (define/public (add-mass! m)
      (set-field! mass (+ m (mass))))

    (define/public (gravity that)
      (* (norm (- (this . loc) (that . loc)))
         (that . mass)
         (this . mass)
         (sqr (/ 1 (magnitude (- (this . loc) (that . loc)))))
         1)) ; (G = 1 works surprisingly well)

    ))

(define-interface point-radius-color<%> [loc vel acc radius color])

(define (circle-mixin %)
  (class (super % point-radius-color<%>)

    (define/public (contains? z)
      (< (magnitude (- z (loc)))
         (radius)))

    ))

;
; Classes
;

(define source%
  ((compose time-mixin circle-mixin point-mixin)
   (class

     (define/public (radius)
       (+ 30 (* 3 (sin (* 2 pi (this . time))))))

     (define/public (color)
       "blue")

     (define/public (draw)
       (circle (radius) "solid" (color)))

     (define/public (click! world)
       (world . add-projectile! (random-projectile-at (this . loc))))

     )))

(define (real->int x)
  (round (inexact->exact x)))

(define sink%
  ((compose mass-mixin time-mixin circle-mixin point-mixin)
   (class

     (define/public (radius)
       (sqrt (/ (this . mass) 2 pi)))

     (define/public (color)
       (make-color (+ 230 (real->int (* 25 (sin (* 2 pi (this . time))))))
                   0
                   0))

     (define/public (draw)
       (circle (radius) "solid" (color)))

     )))

(define projectile%
  ((compose mass-mixin time-mixin point-mixin)
   (class

     (define/public (draw)
       (rotate (modulo (round (* (this . time) 180)) 360)
               (triangle 30 "solid" "black")))

     )))

;
; World
;

; An Object implements: draw, tick!, x, y

(define-class world%
  (fields source projectiles sink)

  (define/public (tick-rate)
    TICK-RATE)

  (define/public (to-draw)
    (foldl (λ (o scn)
             (place-image (o . draw) (o . x) (o . y) scn))
           (empty-scene WIDTH HEIGHT)
           (list* (source) (sink) (projectiles))))

  (define/public (on-tick)
    (begin
      (map (λ (p) (p . acc! ((sink) . gravity p)))
           (projectiles))
      (map (λ (o) (o . tick!))
           (list* (source) (sink) (projectiles)))
      (map (λ (p) (when ((sink) . contains? (p . loc))
                        (begin
                          ((sink) . add-mass! (p . mass))
                          (remove-projectile! p))))
           (projectiles))
      (when (< (random) .05)
        ((source) . click! this))
      this))

  (define/public (on-mouse x y m)
    (begin
      (cond
        [(mouse=? m "button-down")
         (map (λ (o) (if (o . contains? (make-rectangular x y))
                         (o . click! this)
                         (void)))
              (list (source)))]
        [else (void)])
      this))

  (define/public (add-projectile! p)
    (set-field! projectiles (cons p (projectiles))))

  (define/public (remove-projectile! p)
    (set-field! projectiles (remove p (projectiles))))

  )

;
; big-bang
;

(define (random-loc)
  (random-complex-within 0 MAX-LOC))

(define (random-vel)
  (make-polar (random-in-range 20 50)
              (random-in-range 0 (* 2 pi))))

(define (random-vel-fast)
  (* 5 (random-vel)))

(define (random-projectile-at loc)
  (projectile%
    200               ; mass
    0                 ; time
    loc               ; loc
    (random-vel-fast) ; vel
    0))               ; acc

(define (random-source)
  (source%
    0                 ; time
    (random-loc)      ; loc
    (random-vel)      ; vel
    0))               ; acc

(define (random-sink)
  (sink%
    1000              ; mass
    0                 ; time
    (random-loc)      ; loc
    (random-vel)      ; vel
    0))               ; acc

(define (random-world)
  (world% (random-source) empty (random-sink)))

(big-bang (random-world))
