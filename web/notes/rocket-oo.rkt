#lang class0
(require 2htdp/image)
(require class0/universe)

(define ROCKET (bitmap "rocket.png"))

(define WIDTH 100)
(define HEIGHT 300)
(define MT-SCENE (empty-scene WIDTH HEIGHT))

;; A World is a (new rocket% Natural).

(check-expect (send (new rocket% 0) on-tick) (new rocket% 7))
(check-expect (send (new rocket% 0) to-draw)
              (place-image ROCKET (/ WIDTH 2) HEIGHT MT-SCENE))
(define-class rocket%
  (fields h)
  
  ;; -> Scene
  (define/public (to-draw)
    (place-image ROCKET
                 (/ WIDTH 2)
                 (- HEIGHT (field h))
                 MT-SCENE))

  ;; -> World
  (define/public (on-tick)
    (new rocket% (+ (field h) 7))))

(big-bang (new rocket% 0))


