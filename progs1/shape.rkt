#lang class1
(require 2htdp/image)

(define-class posn%
  (fields x y))

(define-class shape%
  (super posn%)
  
  (define/public (draw-on scn)
    (place-image (send this img)
                 (field x)
                 (field y)
                 scn)))

(define-class circle%
  (super shape%)
  (fields r)
  
  (define/public (area)
    (* pi (sqr (field r))))
  
  (define/public (img)
    (circle (field r) "solid" "black")))

(define-class rect%
  (super shape%)
  (fields w h)  
  (define/public (img)
    (rectangle (field w) (field h) "solid" "black")))


  
