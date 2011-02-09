#lang class0
(require 2htdp/image)
(require 2htdp/universe)

(define ROCKET (bitmap "rocket.png"))

(define WIDTH 100)
(define HEIGHT 300)
(define MT-SCENE (empty-scene WIDTH HEIGHT))

;; A World is a Natural.

;; World -> Scene
(check-expect (render 0) (place-image ROCKET (/ WIDTH 2) HEIGHT MT-SCENE))
(define (render h)
  (place-image ROCKET
               (/ WIDTH 2)
               (- HEIGHT h)
               MT-SCENE))

;; World -> World
(check-expect (next 0) 7)
(define (next h)
  (+ h 7))

(big-bang 0
          (on-tick next)
          (to-draw render))

