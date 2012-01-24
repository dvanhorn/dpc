#lang class/0
(provide (all-defined-out))
(require 2htdp/image)

(define LIGHT-RADIUS 20)

(define-class red%
  ;; next : -> Light
  ;; Next light after red
  #;(check-expect (send (new red%) next) (new green%))
  (define (next)
    (new green%))

  ;; draw : -> Image
  ;; Draw this red light
  #;(check-expect (send (new red%) draw)
		(circle LIGHT-RADIUS "solid" "red"))
  (define (draw)
    (circle LIGHT-RADIUS "solid" "red")))

(define-class green%
  ;; next : -> Light
  ;; Next light after green
  #;(check-expect (send (new green%) next) (new yellow%))
  (define (next)
    (new yellow%))

  ;; draw : -> Image
  ;; Draw this green light
  #;(check-expect (send (new green%) draw)
		(circle LIGHT-RADIUS "solid" "green"))
  (define (draw)
    (circle LIGHT-RADIUS "solid" "green")))

(define-class yellow%
  ;; next : -> Light
  ;; Next light after yellow
  #;(check-expect (send (new yellow%) next) (new red%))
  (define (next)
    (new red%))

  ;; draw : -> Image
  ;; Draw this yellow light
  #;(check-expect (send (new yellow%) draw)
		(circle LIGHT-RADIUS "solid" "yellow"))
  (define (draw)
    (circle LIGHT-RADIUS "solid" "yellow")))


 (define-class world%
   (fields light)
   (define (tick-rate) 5)
   (define (to-draw)
     (send (send this light) draw))
   (define (on-tick)
     (new world% (send (send this light) next))))
