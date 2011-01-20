#lang class1
(require 2htdp/image)
(require class0/universe)

(define SIZE 100)
(define 1/2-SIZE (/ SIZE 2))
(define SQ (square SIZE "solid" "black"))

(define R-CIRC (overlay (circle 1/2-SIZE "solid" "red") SQ))
(define Y-CIRC (overlay (circle 1/2-SIZE "solid" "yellow") SQ))
(define G-CIRC (overlay (circle 1/2-SIZE "solid" "green") SQ))

;; A Light implements light<%>.
(define-interface light<%> 
  [;; -> Light
   ;; Produce the light after this light.
   next   
   ;; -> Image
   ;; Draw this light.
   draw])

;; A Light is one of:
;; - (new red%)
;; - (new green%)
;; - (new yellow%)

(define-class red%
  (implements light<%>)
  (define/public (next)
    (new green%))
  (define/public (draw)
    (above R-CIRC SQ SQ)))

(define-class green%
  (implements light<%>)
  (define/public (next)
    (new yellow%))
  (define/public (draw)
    (above SQ SQ G-CIRC)))

(define-class yellow%
  (implements light<%>)
  (define/public (next)
    (new red%))
  (define/public (draw)
    (above SQ Y-CIRC SQ)))

(check-expect (send (new red%) next) (new green%))
(check-expect (send (new green%) next) (new yellow%))
(check-expect (send (new yellow%) next) (new red%))
;; need tests for draw.

;; A Light is a (new light% [0,3)).

(define-class light%
  (implements light<%>)
  (fields n)
  (define/public (next)
    (new light% (modulo (add1 (field n)) 3)))  
  (define/public (draw)
    (cond [(= (field n) 0)
           (above R-CIRC SQ SQ)]
          [(= (field n) 1)
           (above SQ Y-CIRC SQ)]
          [else
           (above SQ SQ G-CIRC)])))

(check-expect (send (new light% 0) next) (new light% 1))
(check-expect (send (new light% 1) next) (new light% 2))
(check-expect (send (new light% 2) next) (new light% 0))
;; need tests for draw.

(define-class world%
  (fields light)
  
  (define/public (tick-rate) 1)
  
  (define/public (on-tick)
    (new world% (send (field light) next)))
  
  (define/public (to-draw)
    (send (field light) draw)))

;(big-bang (new world% (new red%)))
(big-bang (new world% (new light% 0)))
  
  