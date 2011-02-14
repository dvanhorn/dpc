#lang class1
(require class1/universe)
(require 2htdp/image)
(require (only-in lang/htdp-advanced
                  make-hash #;make-immutable-hash))
(require (only-in racket
                  begin
                  dict-ref dict-set dict-update dict-remove dict-map dict-keys
                  dict-set! dict-remove! dict-update!
                  hash-set! hash-remove! hash-update!
                  hash-keys hash-values
                  ;hash make-hash make-immutable-hash
                  set set-add set-remove set-map))
(require (only-in srfi/1
                  delete))

;
; World ------------------------------------------------------------------------
;

; A Location is a Complex where (x,y) is represented as (make-rectangular x y)

(define WIDTH 500)
(define HEIGHT 500)

;
; Blob
;

(define-interface blob<%>
  [loc
   vel
   acc
   draw])

(define-class blob%
  (implements blob<%>)
  (fields data)

  (define/public (draw)
    (circle (this . radius) "solid" (this . color)))

  (define/public (loc)
    (first (field data)))

  (define/public (vel)
    (second (field data)))

  (define/public (acc)
    (third (field data)))

  (define/public (radius)
    (fourth (field data)))

  (define/public (color)
    (fifth (field data)))
  )

;
; World
;

(define-class world%
  (fields blobs)

  (define/public (register)
    LOCALHOST)

  (define/public (to-draw)
    (foldr (λ (b scn) (place-image (b . draw)
                                   (real-part (b . loc))
                                   (imag-part (b . loc))
                                   scn))
           (empty-scene WIDTH HEIGHT)
           (field blobs)))

  (define/public (on-receive blobs-data)
    (new world% (map (λ (d) (new blob% d)) blobs-data)))

  (define/public (on-key k)
    (cond
      [(key=? k "left")  (make-package this -50)]
      [(key=? k "right") (make-package this +50)]
      [(key=? k "up")    (make-package this -50i)]
      [(key=? k "down")  (make-package this +50i)]
      [else              this]))
  )

(define (new-world)
  (new world% empty))

;
; Universe ---------------------------------------------------------------------
;

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
; basic
;

(define MAX-LOC     (make-rectangular WIDTH HEIGHT))
(define LIGHT-SPEED 100)
(define ROCK-RADIUS 30)
(define SHIP-RADIUS 5)
(define NUM-ROCKS   10)
(define TICK-RATE   (exact->inexact 1/20))

(define (random-loc)
  (random-complex-within 0 MAX-LOC))

(define (random-vel)
  (random-complex-within (- (/ MAX-LOC 20)) (/ MAX-LOC 20)))

;
; complex
;

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

(define-interface point-mass<%>
  [loc
   vel
   acc
   with-loc
   with-vel
   with-acc])

(define-class point-mass%
  (fields loc vel acc)

  (define/public (tick)
    (this . with-loc (mod-complex (+ (field loc) (* TICK-RATE (field vel)))
                                  MAX-LOC)
          . with-vel (cap-complex (+ (field vel) (* TICK-RATE (field acc)))
                                  LIGHT-SPEED)))

  (define/public (with-loc l)
    (new point-mass% l (field vel) (field acc)))

  (define/public (with-vel v)
    (new point-mass% (field loc) v (field acc)))

  (define/public (with-acc a)
    (new point-mass% (field loc) (field vel) a))
  )

(define (random-point-mass)
  (new point-mass% (random-loc) (random-vel) 0))

;
; Projectile
;

(define-class projectile%
  (fields point-mass)

  (define/public (loc)
    ((field point-mass) . loc))

  (define/public (vel)
    ((field point-mass) . vel))

  (define/public (acc)
    ((field point-mass) . acc))

  (define/public (with-loc l)
    (this . with-point-mass ((field point-mass) . with-loc l)))

  (define/public (with-vel l)
    (this . with-point-mass ((field point-mass) . with-vel l)))

  (define/public (with-acc l)
    (this . with-point-mass ((field point-mass) . with-acc l)))

  (define/public (tick)
    (this . with-point-mass ((field point-mass) . tick)))
  )

(define (overlap? a b)
  (<= (magnitude (- (a . loc) (b . loc)))
      (+ (a . radius) (b . radius))))

;
; Ship
;

(define-class ship%
  (super projectile%)
  (fields color)

  (define/public (with-point-mass p) (new ship% (field color) p))
  (define/public (with-color c)      (new ship% c (field point-mass)))

  (define/public (radius) SHIP-RADIUS)
  )

(define (random-ship)
  (new ship% (random-color) (random-point-mass)))

;
; Rock
;

(define-class rock%
  (super projectile%)

  (define/public (with-point-mass p) (new rock% p))

  (define/public (radius) ROCK-RADIUS)
  (define/public (color)  "gray")
  )

(define (random-rock)
  (new rock% (random-point-mass)))

;
; Universe
;

(define (just-universe u)
  (make-bundle u empty empty))

(define (hash-map-values h f)
  (make-hash (dict-map h (λ (k v) (list k (f v))))))

(define-class universe%
  (fields ships rocks)

  (define/public (with-ships ss)
    (new universe% ss (field rocks)))

  (define/public (with-rocks rs)
    (new universe% (field ships) rs))

  (define/public (on-new iw)
    (begin
      (hash-set! (field ships) iw (random-ship))
      (just-universe this)))

  (define/public (on-disconnect iw)
    (begin
      (hash-remove! (field ships) iw)
      (just-universe this)))

  (define/public (on-msg iw acc)
    (if (number? acc)
      (begin
        (hash-update! (field ships)
                      iw
                      (λ (s) (s . with-acc acc)))
        (just-universe this))
      (just-universe this)))

  (define/public (on-tick)
    (local [(define tick (λ (x) (send x tick)))]
      (make-bundle
        (this . with-rocks (map tick (field rocks))
              . with-ships (hash-map-values (field ships) tick))
        (this . mails)
        (this . dead-worlds))))

  (define/public (mails)
    (local [(define ships (field ships))
            (define blobs (send this blobs))]
      (map (λ (iw) (make-mail iw (cons (blob (dict-ref (field ships) iw))
                                       blobs)))
           (dict-keys ships))))

  (define/public (blobs)
    (map blob (append (field rocks) (dict-map (field ships) (λ (k v) v)))))

  (define/public (dead-worlds)
    (apply append
           (dict-map (field ships)
                     (λ (iw s)
                       (if (ormap (λ (r) (and (overlap? s r) (not (eq? s r))))
                                  (this . deadly-rocks))
                         (list iw)
                         empty)))))

  (define/public (deadly-rocks)
    (append (field rocks)
            (hash-values (field ships))))

  (define/public (tick-rate)
    TICK-RATE)
  )

(define (blob x)
  (list (x . loc)
        (x . vel)
        (x . acc)
        (x . radius)
        (x . color)))

(define (random-universe)
  (new universe%
       (make-hash empty)
       (repeat NUM-ROCKS random-rock)))

(define (repeat n f)
  (if (<= n 0)
      empty
      (cons (f) (repeat (- n 1) f))))

;
; go
;

(launch-many-worlds
  (universe (random-universe))
  (big-bang (new-world))
  ;(big-bang (new-world))
  ;(big-bang (new-world))
  )
