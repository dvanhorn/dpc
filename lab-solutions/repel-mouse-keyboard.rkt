#lang class1
(require class1/universe)
(require 2htdp/image)

(define WIDTH       500)                             ; px
(define HEIGHT      500)                             ; px
(define MAX-LOC     (make-rectangular WIDTH HEIGHT))
(define LIGHT-SPEED 1000)                            ; px/s
(define TICK-RATE   (exact->inexact 1/20))           ; s

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
  (choose (list "red" "blue" "green" "yellow" "orange" "purple" "black")))

; random-complex-within : Complex -> Complex
; Randomly choose a complex within the rectangle bounded by z1 and z2
(define (random-complex-within z1 z2)
  (make-rectangular (random-between (real-part z1) (real-part z2))
                    (random-between (imag-part z1) (imag-part z2))))

;
; complex
;

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
  (make-polar (min r (magnitude z)) (angle z)))

;
; PointMass
;

(define-class point-mass%
  (fields loc vel acc)

  (define/public (with-loc l)
    (this . with-point-mass l (field vel) (field acc)))

  (define/public (with-vel v)
    (this . with-point-mass (field loc) v (field acc)))

  (define/public (with-acc a)
    (this . with-point-mass (field loc) (field vel) a))

  (define/public (tick)
    (this . with-loc (mod-complex (+ (field loc) (* TICK-RATE (field vel)))
                                  MAX-LOC)
          . with-vel (cap-complex (+ (field vel) (* TICK-RATE (field acc)))
                                  LIGHT-SPEED)))

  (define/public (distance-to that)
    (magnitude (- (this . loc) (that . loc))))

  (define/public (orbit z)
    (this . with-acc (- z (field loc))))

  )

(define (random-loc)
  (random-complex-within 0 MAX-LOC))

(define (random-vel)
  (random-complex-within (- (/ MAX-LOC 20)) (/ MAX-LOC 20)))

;
; Circle
;

(define-class circle%
  (super point-mass%)

  (define/public (draw)
    (circle (this . radius) "solid" (this . color)))

  (define/public (distance-within that)
    (max 0 (- (this . distance-to that)
              (this . radius)
              (that . radius))))

  (define/public (overlap? that)
    (not (positive? (this . distance-within that))))

  (define/public (repulsion that)
    (if (zero? (this . distance-to that))
      0
      (* (norm (- (that . loc) (this . loc)))
         (* 50000 (expt (/ 1 (that . distance-within this))
                        2)))))

  (define/public (repulsion* cs)
    (foldr (λ (c z) (+ z (c . repulsion this))) 0 cs))

  (define/public (repel* cs)
    (this . with-acc (this . repulsion* cs)))

  )

;
; Ship
;

(define-class ship%
  (super circle%)
  (fields color)

  (define/public (make c l v a)
    (new ship% c l v a))

  (define/public (with-point-mass l v a)
    (this . make (field color) l v a))

  (define/public (with-color c)
    (this . make c (field loc) (field vel) (field acc)))

  (define/public (radius) 5)

  )

(define (random-ship)
  (new ship% (random-color) (random-loc) (random-vel) 0))

;
; Rock
;

(define-class rock%
  (super circle%)

  (define/public (make l v a)
    (new rock% l v a))

  (define/public (with-point-mass l v a)
    (this . make l v a))

  (define/public (radius) 20)
  (define/public (color)  "gray")

  )

(define (random-rock)
  (new rock% (random-loc) (random-vel) 0))

;
; World
;

(define (repeat n f)
  (if (<= n 0)
      empty
      (cons (f) (repeat (- n 1) f))))

(define-class world%
  (fields ship rocks orbit)

  (define/public (tick-rate)
    TICK-RATE)

  (define/public (make s rs o)
    (new world% s rs o))

  (define/public (with-ship s)
    (this . make s (field rocks) (field orbit)))

  (define/public (with-rocks rs)
    (this . make (field ship) rs (field orbit)))

  (define/public (with-orbit o)
    (this . make (field ship) (field rocks) o))

  (define/public (to-draw)
    (foldr (λ (b scn)
             (let* ([x (real-part (b . loc))]
                    [y (imag-part (b . loc))]
                    [z (b . repulsion (field ship))]
                    [x′ (+ x (real-part z))]
                    [y′ (+ y (imag-part z))])
               (scene+line (place-image (b . draw) x y scn)
                           x y x′ y′ "red")))
           (empty-scene WIDTH HEIGHT)
           (cons (field ship) (field rocks))))

  (define/public (on-tick)
    (this . with-ship  ((if (number? (field orbit))
                          ((field ship) . orbit (field orbit))
                          ((field ship) . repel* (field rocks)))
                        . tick)
          . with-rocks (map (λ (r) (r . tick)) (field rocks))))

  (define/public (stop-when)
    (ormap (λ (r) (r . overlap? (field ship))) (field rocks)))

  (define/public (on-mouse x y m)
    (cond
      [(or (mouse=? m "button-down")
           (mouse=? m "drag"))
       (this . with-orbit (make-rectangular x y))]
      [(mouse=? m "button-up")
       (this . with-orbit false)]
      [else this]))

  (define/public (on-key k)
    (cond
      [(key=? k "left")  (this . with-ship ((field ship) . with-acc -10))]
      [(key=? k "right") (this . with-ship ((field ship) . with-acc +10))]
      [(key=? k "up")    (this . with-ship ((field ship) . with-acc -10i))]
      [(key=? k "down")  (this . with-ship ((field ship) . with-acc +10i))]
      [(key=? k " ")     (this . with-ship ((field ship) . with-acc 0))]
      [(key=? k "q")     (stop-with this)]
      [else              this]))

  )

(define (random-world)
  (new world% (random-ship) (repeat 10 random-rock) false))

;
; go
;

(big-bang (random-world))
