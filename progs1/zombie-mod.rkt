#lang class1
(require "zombie.rkt")
;; ==========================================================
;; Play the classic game of Zombie Attack!

;; All zombies move toward the player.  The player moves 
;; toward the mouse. Zombies collision cause flesh heaps 
;; that are deadly to other zombies and the player.  
;; Randomly teleport via mouse click as a last resort!

;; Based on Robot!, p. 234 of Barski's Land of Lisp.
(require 2htdp/image)
(require class1/universe)

;; ==========================================================
;; A Player is one of:
;; - a (new player% [0,WIDTH] [0,HEIGHT]).
;; - a (new modulo-player% [0,WIDTH] [0,HEIGHT]).

(define-class modulo-player%
  (super being%)
  (implements player<%>)
  
  (define/public (color) "orange")  
  
  (define/public (move-toward mouse)
    (plus (modulo-min-taxi P-SPEED mouse)))

  (define/public (plus v)
    (new modulo-player%
         (modulo (+ (field x) (send v x)) WIDTH)
         (modulo (+ (field y) (send v y)) HEIGHT)))
  
  (define/public (place-at x y)
    (new modulo-player% x y)))


;; ==========================================================
;; A Zombie is one of:
;; - LiveZombie 
;; - DeadZombie
;; - ModuloLiveZombie

;; A DeadZombie is a (new dead-zombie% [0,WIDTH] [0,HEIGHT]).
;; A LiveZombie is a (new live-zombie% [0,WIDTH] [0,HEIGHT]).
;; A ModuloLiveZombie is a 
;;   (new modulo-live-zombie% [0,WIDTH] [0,HEIGHT]).

(define-class modulo-live-zombie% 
  (super zombie%)
  (implements zombie<%>)
  
  (define/public (move-toward p)
    (plus (modulo-min-taxi Z-SPEED p)))
    
  (define/public (color) "pink")
  
  (define/public (plus v)
    (new modulo-live-zombie% 
         (modulo (+ (field x) (send v x)) WIDTH)
         (modulo (+ (field y) (send v y)) HEIGHT))))


;; ==========================================================
;; Run program, run!
(big-bang
 (new world%
      (new modulo-player% (/ WIDTH 2) (/ HEIGHT 2))
      (build-loz (+ 10 (random 20))
                 (λ (_)
                   (new modulo-live-zombie% 
                        (random WIDTH)
                        (random HEIGHT))))
      (new mouse% 0 0)))


;; ==========================================================
;; Test cases

(define ml0 (new modulo-live-zombie% 0 0))
(define mp0 (new modulo-player% 0 0))
(define mp1 (new modulo-player% 0 30))
(define mw0 (new world% mp0 mt m0))
(define mw7 (send mw0 teleport))

(define pR (new player% (- WIDTH Z-SPEED) 0))
(define mR (new mouse%  (- WIDTH P-SPEED) 0))

;; World tests
;; ===========

;; teleport
(check-within mw7 mw0 (+ WIDTH HEIGHT))

;; Player tests
;; ============

;; draw-on
(check-expect (send mp0 draw-on MT-SCENE)
              (place-image (circle 1/2-CELL "solid" "orange")
                           0 0
                           MT-SCENE))
;; plus
(check-expect (send mp0 plus (new vec% 0 0)) mp0)
(check-expect (send mp0 plus (new vec% 0 30)) mp1)
;; move-toward
(check-expect (send mp0 move-toward m0) mp0)
(check-expect (send mp0 move-toward m1)
              (new modulo-player% 0 P-SPEED))
(check-expect (send mp0 move-toward mR)
              (new modulo-player% (- WIDTH P-SPEED) 0))
;; dist
(check-expect (send mp0 dist m1) 30)
;; min-taxi
(check-expect (send mp0 min-taxi 1 m0) (new vec% 0 0))
(check-expect (send mp0 min-taxi 1 m1) (new vec% 0 1))
(check-expect (send mp0 min-taxi 1 mR) (new vec% -1 0))

;; Zombie tests
;; ============

;; move-toward
(check-expect (send ml0 move-toward p0) ml0)
(check-expect (send ml0 move-toward p1)
              (new modulo-live-zombie% 0 Z-SPEED))
(check-expect (send ml0 move-toward pR)
              (new modulo-live-zombie% (- WIDTH Z-SPEED) 0))
;; draw-on
(check-expect (send ml0 draw-on MT-SCENE)
              (place-image (circle 1/2-CELL "solid" "pink")
                           0 0
                           MT-SCENE))

;; dist
(check-expect (send ml0 dist p1) 30)
;; min-taxi
(check-expect (send ml0 min-taxi 5 p0) (new vec% 0 0))
(check-expect (send ml0 min-taxi 5 p1) (new vec% 0 5))
(check-expect (send ml0 min-taxi 5 pR) (new vec% -5 0))
