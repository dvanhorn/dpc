#lang racket
(provide (all-defined-out))
;; Play the classic game of Zombie Brains!
;; All zombies move towards the *closest* living player.  Zombies collision 
;; cause flesh heaps that are deadly to other zombies (and you!).  Teleport
;; as a last resort!

;; Based on Robot!, p. 234 of Barski's Land of Lisp.
(require 2htdp/universe)
(require 2htdp/image)
(require "shared.rkt")
  
;; A Zombie is a Meat.
;; A Player is a Meat.
;; A Meat is one of:         ; interp:
;; - (+ (- Nat) (* +i Nat))  ; dead meat
;; - (+ (+ Nat) (* +i Nat))  ; live meat
;; A Posn is a (+ Nat (* +i Nat)).
;; A World is a (list Player [Listof Player] [Listof Zombie]).

;; Host World -> World
(define (play host w)
  (big-bang w
            (register host)
            (on-draw draw)
            (on-mouse squeak)
            (on-receive (λ (w m) m))))

;; World -> Scene
(define (draw w)
  (foldl (λ (o scn) ; Player Scene -> Scene
           ((posn+scn (if (dead? o) "black" "orange"))
            (pos-abs o)
            scn))
         (foldl (λ (z scn) ; Zombie Scene -> Scene
                  ((posn+scn (if (dead? z) "gray" "red"))
                   (pos-abs z)
                   scn))
                ((posn+scn (if (dead? (player w)) "purple" "green"))
                 (pos-abs (player w))
                 (empty-scene (pos-x *dim*)
                              (pos-y *dim*)))
                (zombies w))
         (opponents w)))

;; World Int Int Mouse -> World
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
