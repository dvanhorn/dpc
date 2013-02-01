#lang class/1
(require 2htdp/image)

;; A Shape is one of:
;; - (new circ% +Real Real Real)
;; - (new rect% +Real +Real Real Real)
;; A +Real is a positive, real number.
(define-class shape%
  (fields x y)
  
  ;; Scene -> Scene
  ;; Draw this shape on the scene.
  (define (draw-on scn)
    (place-image (send this img)
                 (this . x)
                 (this . y)
                 scn)))

(define-class circ%
  (super shape%)
  (fields radius)
  
  ;; -> +Real
  (define (area)
    (* pi (sqr (this . radius))))
  
  ;; -> Image
  ;; Render this circle as an image.
  (define (img)
    (circle (this . radius) "solid" "black")))

(define-class rect%
  (super shape%)
  (fields width height) 
  
  ;; -> +Real
  ;; Compute the area of this rectangle.
  (define (area)
    (* (this . width)
       (this . height)))
  
  ;; -> Image
  ;; Render this rectangle as an image.
  (define (img)
    (rectangle (this . width) (this . height) "solid" "black")))


  
