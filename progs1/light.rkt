#lang class1
(require 2htdp/image)
(require class1/universe)

(define SIZE 100)
(define 1/2-SIZE (/ SIZE 2))
(define SQ (square SIZE "solid" "black"))

(define R-CIRC (overlay (circle 1/2-SIZE "solid" "red") SQ))
(define Y-CIRC (overlay (circle 1/2-SIZE "solid" "yellow") SQ))
(define G-CIRC (overlay (circle 1/2-SIZE "solid" "green") SQ))

(define R-LIGHT (above R-CIRC SQ SQ))
(define Y-LIGHT (above SQ Y-CIRC SQ))
(define G-LIGHT (above SQ SQ G-CIRC))

;; A Light implements light<%>.
(define-interface light<%> 
  [;; -> Light
   ;; Produce the light after this light.
   next   
   ;; -> Image
   ;; Draw this light.
   draw])

;; A Light1 is one of:
;; - (new red%)
;; - (new green%)
;; - (new yellow%)

(define-class red%
  (implements light<%>)
  (define/public (next) (new green%))
  (define/public (draw) R-LIGHT))

(define-class green%
  (implements light<%>)
  (define/public (next) (new yellow%))
  (define/public (draw) G-LIGHT))

(define-class yellow%
  (implements light<%>)
  (define/public (next) (new red%))
  (define/public (draw) Y-LIGHT))

(check-expect (send (new red%) next) (new green%))
(check-expect (send (new green%) next) (new yellow%))
(check-expect (send (new yellow%) next) (new red%))

;; A Light2 is a (new light% [0,3)).
;; Interp: 0 is red, 1 is yellow, 2 is green.
(define-class light%
  (implements light<%>)
  (fields n)
  (define/public (next)
    (new light% (modulo (add1 (field n)) 3)))  
  (define/public (draw)
    (cond [(= (field n) 0)
           (above R-CIRC SQ SQ)]
          [(= (field n) 1)
           (above SQ SQ G-CIRC)]
          [else
           (above SQ Y-CIRC SQ)])))

(check-expect (send (new light% 0) next) (new light% 1))
(check-expect (send (new light% 1) next) (new light% 2))
(check-expect (send (new light% 2) next) (new light% 0))

;; A World is a (new world% Light).
(define-class world%
  (fields light)
  
  (define/public (tick-rate) 1)
  
  (define/public (on-tick)
    (new world% (send (field light) next)))
  
  (define/public (to-draw)
    (send (field light) draw)))

(launch-many-worlds (big-bang (new world% (new red%)))
                    (big-bang (new world% (new light% 0))))
  
  