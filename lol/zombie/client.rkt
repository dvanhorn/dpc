#lang racket
;; Zombie CLIENT

;; Play the classic game of Zombie Brains!
;; All zombies move towards the *closest* living player.  Zombies collision 
;; cause flesh heaps that are deadly to other zombies (and you!).  Teleport
;; as a last resort!

;; On mouse events, the client sends requests to move to the server.
;; On receive events, the client installs the world state sent by the server.

;; Based on Robot!, p. 234 of Barski's Land of Lisp.
(provide (all-defined-out))
(require 2htdp/universe)
(require 2htdp/image)
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

;; World -> Scene
;; Render the world state.
(define (draw w)
  (foldl (λ (o scn) ; Player Scene -> Scene
           ((posn+scn (if (dead? o) "black" "orange"))
            (meat-pos o)
            scn))
         (foldl (λ (z scn) ; Zombie Scene -> Scene
                  ((posn+scn (if (dead? z) "gray" "red"))
                   (meat-pos z)
                   scn))
                ((posn+scn (if (dead? (player w)) "purple" "green"))
                 (meat-pos (player w))
                 (empty-scene (pos-x *dim*)
                              (pos-y *dim*)))
                (zombies w))
         (opponents w)))

;; World Int Int Mouse -> World
;; Send a request to move to the server.
(define (squeak w x y m)
  (make-package w
                ((cond [(mouse=? "button-down" m) teleport]
                       [else toward])
                 (pos x y))))

;; Color -> Posn Scene -> Scene
;; Render the posn on the scene in the given color.
(define ((posn+scn c) p s)
  (place-image (circle (/ *cell-size* 2) "solid" c)
               (pos-x p)
               (pos-y p)
               s)) 

(define player first)
(define opponents second)
(define zombies third)
