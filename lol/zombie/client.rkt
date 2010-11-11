;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname client) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ISL+λ
;; Zombie CLIENT

;; Play the classic game of Zombie Brains!
;; All zombies move towards the *closest* living player.  Zombies collision 
;; cause flesh heaps that are deadly to other zombies (and you!).  Teleport
;; as a last resort!

;; On mouse events, the client sends requests to move to the server.
;; On receive events, the client installs the world state sent by the server.

;; Based on Robot!, p. 234 of Barski's Land of Lisp.
(require class0)
(provide (all-defined-out))
(require 2htdp/universe)
(require 2htdp/image)    ; Duplicates 'image?'
                         ; I think this is still a symptom of not having racket's require.
(require test-engine/scheme-tests)
(require "shared.rkt")

;; A World is a (list Player [Listof Player] [Listof Zombie]).
;; Interp: you, your opponents, & the zombies.

;; Host -> World
;; Join a game on the given host.
(define (play host)
  (big-bang (list 0 (list) (list))
            (register host)
            (on-draw draw)
            (on-mouse squeak)
            (on-receive (λ (w m) m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering
(define *mt-scene*
  (empty-scene (pos-x *dim*)
               (pos-y *dim*)))

;; World -> Scene
;; Render the world state.
(define (draw w)
  (opponents+scn (opponents w)
                 (zombies+scn (zombies w)
                              (player+scn (player w)
                                          *mt-scene*))))

(check-expect (draw (list 0 (list 5) (list 8)))
              (opponents+scn (list 5)
                             (zombies+scn (list 8)
                                          (player+scn 0
                                                      *mt-scene*))))

;; [Listof Players] Scene -> Scene
(define (opponents+scn os scn)
  (foldl (color-meat "black" "orange")
         scn
         os))

(check-expect (opponents+scn (list 30+30i) *mt-scene*)
              ((posn+scn "orange") 30+30i *mt-scene*))
(check-expect (opponents+scn (list (dead 30+30i)) *mt-scene*)
              ((posn+scn "black") 30+30i *mt-scene*))

;; [Listof Zombies] Scene -> Scene
(define (zombies+scn zs scn)
  (foldl (color-meat "gray" "red")
         scn
         zs))

(check-expect (zombies+scn (list 30+30i) *mt-scene*)
              ((posn+scn "red") 30+30i *mt-scene*))
(check-expect (zombies+scn (list (dead 30+30i)) *mt-scene*)
              ((posn+scn "gray") 30+30i *mt-scene*))

;; Player Scene -> Scene
(define (player+scn p scn)
  ((color-meat "purple" "green") p scn))

(check-expect (player+scn 30+30i *mt-scene*)
              ((posn+scn "green") 30+30i *mt-scene*))
(check-expect (player+scn (dead 30+30i) *mt-scene*)
              ((posn+scn "purple") 30+30i *mt-scene*))

;; Color Color -> [Meat Scene -> Scene]
;; Constructs a "painter" for meat that uses the given colors.
(define (color-meat dead-c live-c)
  (λ (m scn)
    ((posn+scn (if (dead? m) dead-c live-c))
     (meat-pos m)
     scn)))

(check-expect ((color-meat "red" "blue") (dead 0) *mt-scene*)
              ((posn+scn "red") 0 *mt-scene*))
(check-expect ((color-meat "red" "blue") 0 *mt-scene*)
              ((posn+scn "blue") 0 *mt-scene*))

;; Color -> Posn Scene -> Scene
;; Render the posn on the scene in the given color.
(define ((posn+scn c) p s)
  (place-image (circle (/ *cell-size* 2) "solid" c)
               (pos-x p)
               (pos-y p)
               s))

(check-expect ((posn+scn "red") 30+40i *mt-scene*)
              (place-image (circle (/ *cell-size* 2) "solid" "red")
                           30
                           40
                           *mt-scene*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse handling

;; World Int Int Mouse -> World
;; Send a request to move to the server.
(define (squeak w x y m)
  (make-package w
                ((cond [(mouse=? "button-down" m) teleport]
                       [else toward])
                 (pos x y))))

(check-expect (squeak 5 3 4 "button-down")
              (make-package 5 (teleport 3+4i)))
(check-expect (squeak 5 3 4 "move")
              (make-package 5 (toward 3+4i)))

(define player first)
(define opponents second)
(define zombies third)

;(test)

