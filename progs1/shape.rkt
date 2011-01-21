#lang class1
(require 2htdp/image)

;; A Shape is one of:
;; - (new circ% +Real Real Real)
;; - (new rect% +Real +Real Real Real)
;; A +Real is a positive, real number.
(define-class shape%
  (fields x y)
  
  ;; Scene -> Scene
  ;; Draw this shape on the scene.
  (define/public (draw-on scn)
    (place-image (send this img)
                 (field x)
                 (field y)
                 scn)))

(define-class circ%
  (super shape%)
  (fields radius)
  
  ;; -> +Real
  (define/public (area)
    (* pi (sqr (field radius))))
  
  ;; -> Image
  ;; Render this circle as an image.
  (define/public (img)
    (circle (field radius) "solid" "black")))

(define-class rect%
  (super shape%)
  (fields width height) 
  
  ;; -> +Real
  ;; Compute the area of this rectangle.
  (define/public (area)
    (* (field width)
       (field height)))
  
  ;; -> Image
  ;; Render this rectangle as an image.
  (define/public (img)
    (rectangle (field width) (field height) "solid" "black")))


  
