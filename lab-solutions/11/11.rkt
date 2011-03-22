#lang class4
(require "class4/blobs.rkt")
(require class4/universe)
(require 2htdp/image)

;
; Blob
;

(define-class blob%
  (super base-blob%)

  ; 5.

  (define/public (collide! that)
    (if (>= (this . area) (that . area))
      (this . eat! that)
      (that . eat! this)))

  ;(define/public (eat! that)
  ;  (set-field! area (+ (this . area) (that . take!))))
  ;(define/public (take!)
  ;  (begin
  ;    (define A (this . area))
  ;    (set-field! area 0)
  ;    A))

  ; 7.

  (define/public (set-acc! a)
    (set-field! acc a))

  ; 9.

  (define/public (eat! that)
    (set-field! area
                (+ (this . area)
                   (that . take! (* TICK-RATE
                                    (sqrt (/ (this . area) (that . area)))
                                    1000)))))
  (define/public (take! A)
    (let ([dA (min (this . area) A)])
      (begin
        (set-field! area (- (this . area) dA))
        dA)))

  )

;
; World
;

(define-class world%
  (super base-world%)

  ; 7.

  (define/public (accelerate-player! a)
    ((this . player) . set-acc! a))

  ;(define/public (on-key k)
  ;  (cond
  ;    [(key=? k "q")     (stop-with this)]
  ;    [(key=? k "left")  (begin (accelerate-player! -50)  this)]
  ;    [(key=? k "right") (begin (accelerate-player! +50)  this)]
  ;    [(key=? k "up")    (begin (accelerate-player! -50i) this)]
  ;    [(key=? k "down")  (begin (accelerate-player! +50i) this)]))

  ; 8.

  (define/public (on-key-change ks)
    (begin
      (accelerate-player! (+ (if (key-in? "left"  ks) -50  0)
                             (if (key-in? "right" ks) +50  0)
                             (if (key-in? "up"    ks) -50i 0)
                             (if (key-in? "down"  ks) +50i 0)))
      (if (key-in? "q" ks)
        (stop-with this)
        this)))

  )

;
; go
;

(define (repeat n f)
  (if (<= n 0)
      empty
      (cons (f) (repeat (- n 1) f))))

(define (random-loc)
  (random-complex-within 0 500+500i))

(define (random-vel)
  (random-complex-within -25-25i 25+25i))

(define (random-area)
  (random-between 500 2500))

(define (random-player)
  (blob% (random-loc) (random-vel) 0 (* 1.5 (random-area)) "black"))

(define (random-enemy)
  (blob% (random-loc) (random-vel) 0 (random-area) (random-color)))

(define (random-world)
  (world% (random-player) (repeat 20 random-enemy)))

(big-bang (random-world))
