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
(require (except-in 2htdp/image image?)) ; This is unfortunate.
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
  (foldl (meat+scn dead-opponent-img live-opponent-img) 
         scn
         os))

(check-expect (opponents+scn (list 30+30i) *mt-scene*)
              (place-image live-opponent-img 30 30 *mt-scene*))
(check-expect (opponents+scn (list (dead 30+30i)) *mt-scene*)
              (place-image dead-opponent-img 30 30 *mt-scene*))
       
;; [Listof Zombies] Scene -> Scene
(define (zombies+scn zs scn)
  (foldl (meat+scn dead-zombie-img live-zombie-img) 
         scn 
         zs))

(check-expect (zombies+scn (list 30+30i) *mt-scene*)
              (place-image live-zombie-img 30 30 *mt-scene*))
(check-expect (zombies+scn (list (dead 30+30i)) *mt-scene*)
              (place-image dead-zombie-img 30 30 *mt-scene*))

;; Player Scene -> Scene
(define (player+scn p scn)
  ((meat+scn dead-player-img live-player-img) p scn))

(check-expect (player+scn 30+30i *mt-scene*)
              (place-image live-player-img 30 30 *mt-scene*))
(check-expect (player+scn (dead 30+30i) *mt-scene*)
              (place-image dead-player-img 30 30 *mt-scene*))

;; Image Image -> [Meat Scene -> Scene]
(define ((meat+scn dead-img live-img) m scn)
  (place-image (if (dead? m) dead-img live-img)
               (pos-x (meat-pos m))
               (pos-y (meat-pos m))
               scn))

(define (meat-img eye-color skin-color)
  (local [(define face-width 30)
          (define face-height 25)   
          (define eye 
            (overlay (ellipse 6 4 "solid" eye-color)
                     (ellipse 12 8 "solid" "black")))]
    (overlay (beside eye eye)
             (ellipse face-width face-height "solid" skin-color)
             (ellipse (+ face-width 4) (+ face-height 4) "solid" "black"))))

(define dead-player-img (meat-img 'white 'white))
(define live-player-img (meat-img 'cornflowerblue 'pink))
(define dead-zombie-img (meat-img 'darkgray 'darkgray))
(define live-zombie-img (meat-img 'yellow 'yellowgreen))
(define dead-opponent-img dead-player-img)
(define live-opponent-img (meat-img 'navajowhite 'brown))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse handling

;; World Int Int Mouse -> [U World Package]
;; Maybe send a request to move to the server.
(define (squeak w x y m)
  (cond [(mouse=? "button-down" m)
         (make-package w
                       (teleport (pos x y)))]
        [else
         (cond [(even? (+ x y)) ; coin flip
                (make-package w 
                              (toward (pos x y)))]
               [else w])]))

(check-expect (squeak 5 3 4 "button-down")
              (make-package 5 (teleport 3+4i)))
(check-expect (squeak 5 3 4 "move")
              5)
(check-expect (squeak 5 3 7 "move")
              (make-package 5 (toward 3+7i)))

(define player first)
(define opponents second)
(define zombies third)

