#lang racket

(require class0)
(require 2htdp/image)
(require 2htdp/universe)

; A Ball is a (new ball% Number Number Number Color)
(define-class ball%
  (fields x y radius color)

  ; -> Number
  ; The diameter of the Ball
  (define/public (diameter)
    (* 2 pi (field radius)))

  ; Number -> Ball
  ; Scale the radius of the Ball by a factor of x
  (define/public (scale-radius x)
    (new ball% (field x) (field y) (* x (field radius)) (field color)))

  ; -> Image
  ; The image representing the Ball
  (define/public (draw)
    (circle (field radius) 'solid (field color)))

  ; -> Ball
  ; The next Ball in the animation sequence
  (define/public (step)
    (scale-radius 49/50)))

; A Block is a (new block% Number Number Number Number Color)
(define-class block%
  (fields x y width height color)

  ; -> Number
  ; The length of the diagonal of the Block
  (define/public (diagonal)
    (sqrt (+ (sqr (field width)) (sqr (field height)))))

  ; -> Image
  ; The image representing the Block
  (define/public (draw)
    (rectangle (field width) (field height) 'solid (field color)))

  ; -> Block
  ; The next Block in the animation sequence
  (define/public (step)
    (new block%
         (field x)
         (+ 1 (field y))
         (field width)
         (field height)
         (field color))))

; random-between : Integer Integer -> Integer
; Randomly choose a number between a and b (inclusive)
(define (random-between a b)
  (+ b (random (+ 1 (- b a)))))

; choose : [Listof X] -> X
; Randomly choose an element from the list
(define (choose xs)
  (list-ref xs (random (length xs))))

; random-color : -> Color
; Randomly choose a color from a set of common colors
(define (random-color)
  (choose (list "red" "blue" "green" "yellow" "orange" "purple" "black")))

; spawn-ball : Number Number -> Ball
; Create a Ball with random parameters at location (x,y)
(define (spawn-ball x y)
  (new ball% x y (random-between 10 20) (random-color)))

; spawn-block : Number Number -> Block
(define (spawn-block x y)
  (new block% x y (random-between 10 20) (random-between 10 20) (random-color)))

; A Creature is an object that understands the `x', `y', `draw', and `step'
; messages

; spawn-functions : [Listof (-> Creature)]
(define spawn-functions
  (list spawn-ball spawn-block))

; creature-spawner : -> (Number Number -> Creature)
(define (creature-spawner)
  (choose (list spawn-ball spawn-block)))

; A World is a [Listof Creature]

; tick : World -> World
; Advance the World
(define (tick w)
  (map (λ (c) (send c step)) w))

; draw : World -> Image
; Draw the World
(define (draw w)
  (foldr (λ (c scn) (place-image (send c draw)
                                 (send c x)
                                 (send c y)
                                 scn))
         (empty-scene 400 400)
         w))

; mouse : World Number Number MouseEvent -> World
; Respond to mouse events
(define (mouse w x y ev)
  (cond
    [(equal? ev "button-down")
     (cons ((creature-spawner) x y) w)]
    [else w]))

(big-bang empty
          (on-tick tick)
          (on-draw draw)
          (on-mouse mouse))