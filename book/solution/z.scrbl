#lang scribble/manual
@(require class/utils
          (for-label (except-in class/0 check-expect))
          (for-label (only-in lang/htdp-intermediate-lambda check-expect)))

@title[#:tag "Zombie_solution"]{Solution:
@secref{Zombie_}}

This is a solution for the @secref{Zombie_} exercise.

@#reader scribble/comment-reader
(racketmod
class/0
(require 2htdp/image)
(require class/universe)

;; A Zombie is one of:
;; - Undead
;; - Dead
;; implements Point and:
;; - move-toward : Player -> Zombie
;; - eat-zombie-brains? : Zombie -> Boolean
;; - eat-player-brains? : Player -> Boolean
;; - draw-on : Scene -> Scene

;; A Undead is a (new undead% ...)
;; A Dead is a (new dead% ...)

;; A Zombies is one of:
;; - (new emptyz%)
;; - (new conz% Zombie Zombies)
;; implements
;; - draw-on : Scene -> Scene

;; A Player is (new player% Posn)
;; implements Point and
;; - move-toward : Mouse -> Player
;; - draw-on : Scene -> Scene

;; A Point implements
;; - x : -> Number
;; - y : -> Number

;; A Posn is a (new posn% Number Number)
;; implements Point and
;; - move-toward : Point -> Posn
;; - draw-on/image : Image Scene -> Scene

;; A Mouse is a (new mouse% Number Number)
;; implements
;; draw-on : Scene -> Scene
;; Draws this mouse on the scene

(define PLAYER-VELOCITY 3)
(define ZOMBIE-VELOCITY 1)
(define PLAYER-RADIUS 10)
(define ZOMBIE-RADIUS 10)

(define-class emptyz%
  (define (draw-on scn) scn)
  (define (move-toward player) this)
  (define (any-eating-player-brains? player) false)  
  (define (any-eating-zombie-brains? zombie) false)
  (define (kill-any-touching) this)  
  (define (kill-any-touching/a zs) zs))

(define-class conz%
  (fields first rest)
  (define (draw-on scn)
    (send (send this first) draw-on
          (send (send this rest) draw-on scn)))
  
  (define (move-toward player)
    (new conz% 
         (send (send this first) move-toward player)
         (send (send this rest) move-toward player)))
  
  (define (any-eating-player-brains? player)
    (or (send (send this first) eat-player-brains? player)
        (send (send this rest) any-eating-player-brains? player)))
  
  (define (any-eating-zombie-brains? zombie)
    (or (send (send this first) eat-zombie-brains? zombie)
        (send (send this rest) any-eating-zombie-brains? zombie)))
  
  (define (kill-any-touching)
    (send this kill-any-touching/a (new emptyz%)))
  
  (define (kill-any-touching/a zs)
    (local [(define first-z (send this first))
            (define rest-z (send this rest))]
      (cond [(or (send zs any-eating-zombie-brains? first-z)
                 (send rest-z any-eating-zombie-brains? first-z))
             (send rest-z kill-any-touching/a (new conz% (send first-z kill) zs))]
            [else
             (send rest-z kill-any-touching/a (new conz% first-z zs))]))))
  

;; A Undead is a (new undead% Posn) implements Zombie
(define-class undead%
  (fields p)
  ;; - move-toward : Player -> Zombie
  ;; - eat-zombie-brains? : Zombie -> Boolean
  ;; - eat-player-brains? : Player -> Boolean  
  
  (define (x) (send (send this p) x))
  (define (y) (send (send this p) y))
  
  (check-expect (send (new undead% (new posn% 10 20)) draw-on (empty-scene 100 100))
                (place-image (circle ZOMBIE-RADIUS "solid" "red")
                             10 20
                             (empty-scene 100 100)))
  (define (draw-on scn)
    (send (send this p) draw-on/image (circle ZOMBIE-RADIUS "solid" "red") scn))
  
  (check-expect (send (new undead% (new posn% 0 0)) move-toward 
                      (new player% (new posn% 0 10)))
                (new undead% (new posn% 0 ZOMBIE-VELOCITY)))
  (check-expect (send (new undead% (new posn% 0 0)) move-toward 
                      (new player% (new posn% 10 0)))
                (new undead% (new posn% ZOMBIE-VELOCITY 0)))
  
  ;; move-toward : Player -> Zombie
  ;; Move this player ZOMBIE-VELOCITY units toward the given player.
  (define (move-toward player)
    (new undead% (send (send this p) move-toward player ZOMBIE-VELOCITY)))   
  
  ;; eat-player-brains? : Player -> Boolean
  (define (eat-player-brains? player)
    (< (send (send this p) dist player)
       (+ ZOMBIE-RADIUS PLAYER-RADIUS)))
  
  ;; eat-zombie-brains? : Zombie -> Boolean
  (define (eat-zombie-brains? zombie)
    (< (send (send this p) dist zombie)
       (* 2 ZOMBIE-RADIUS)))
  
  ;; kill : -> Dead
  (define (kill)
    (new dead% (send this p))))
  

;; A Dead is a (new dead% ...) implements Zombie
(define-class dead%
  (fields p)
    
  (define (x) (send (send this p) x))
  (define (y) (send (send this p) y))
 
  ;; - move-toward : Player -> Zombie
  ;; - eat-zombie-brains? : Zombie -> Boolean
  ;; - eat-player-brains? : Player -> Boolean  
  
  (check-expect (send (new dead% (new posn% 10 20)) draw-on (empty-scene 100 100))
                (place-image (circle ZOMBIE-RADIUS "solid" "gray")
                             10 20
                             (empty-scene 100 100)))
  (define (draw-on scn)
    (send (send this p) draw-on/image (circle ZOMBIE-RADIUS "solid" "gray") scn))
  
  (check-expect (send (new dead% (new posn% 0 0)) move-toward 
                      (new player% (new posn% 0 10)))
                (new dead% (new posn% 0 0)))
  (check-expect (send (new dead% (new posn% 0 0)) move-toward 
                      (new player% (new posn% 10 0)))
                (new dead% (new posn% 0 0)))
  (define (move-toward player) 
    this)
  
  ;; eat-player-brains? : Player -> Boolean
  (define (eat-player-brains? player)
    (< (send (send this p) dist player)
       (+ ZOMBIE-RADIUS PLAYER-RADIUS)))
  
  ;; eat-zombie-brains? : Zombie -> Boolean
  (define (eat-zombie-brains? zombie)
    (< (send (send this p) dist zombie)
       (* 2 ZOMBIE-RADIUS)))
  
  ;; kill : -> Dead
  (define (kill)
    this))


(define-class posn%
  (fields x y)
  ;; Move this position toward that one at given velocity.
  ;; move-toward : Point Number -> Posn
  (define (move-toward that velocity)        
    (local [(define delta-x (- (send that x) (send this x)))
            (define delta-y (- (send that y) (send this y)))
            (define move-distance
              (min velocity
                   (max (abs delta-x)
                        (abs delta-y))))]
      
      (cond [(< (abs delta-x) (abs delta-y))
             ;; move along y-axis
             (cond [(positive? delta-y)
                    (send this move 0 move-distance)]
                   [else
                    (send this move 0 (- move-distance))])]                          
            [else
             ;; move along x-axis
             (cond [(positive? delta-x)
                    (send this move move-distance 0)]
                   [else
                    (send this move (- move-distance) 0)])])))
  
  ;; move : Number Number -> Posn
  (define (move delta-x delta-y)
    (new posn%
         (+ (send this x) delta-x)
         (+ (send this y) delta-y)))
  
  ;; draw-on/image : Image Scene -> Scene
  (define (draw-on/image img scn)
    (place-image img
                 (send this x)
                 (send this y)
                 scn))
  
  ;; dist : Point -> Number
  ;; Compute the distance between this posn and that point.
  (define (dist that)
    (sqrt (+ (sqr (- (send that y) (send this y)))
             (sqr (- (send that x) (send this x)))))))
    

(define-class player%
  (fields p)
  (define (x) (send (send this p) x))
  (define (y) (send (send this p) y))
  
  ;; move-toward : Mouse -> Player
  ;; Move this player PLAYER-VELOCITY units toward the given mouse.
  (define (move-toward mouse)
    (new player% 
         (send (send this p) move-toward mouse PLAYER-VELOCITY)))
  
  (check-expect (send (new player% (new posn% 10 20)) draw-on (empty-scene 100 100))
                (place-image (circle PLAYER-RADIUS "solid" "green")
                             10 20
                             (empty-scene 100 100)))
  (define (draw-on scn)
    (send (send this p) draw-on/image (circle PLAYER-RADIUS "solid" "green") scn)))
  
(check-expect (send (new posn% 0 0) move-toward (new posn% 0 0) 100)
              (new posn% 0 0))
(check-expect (send (new posn% 0 0) move-toward (new posn% 100 0) 50)
              (new posn% 50 0))
(check-expect (send (new posn% 0 0) move-toward (new posn% 0 100) 50)
              (new posn% 0 50))
(check-expect (send (new posn% 0 0) move-toward (new posn% 100 100) 50)
              (new posn% 50 0))
(check-expect (send (new posn% 0 0) move-toward (new posn% 100 101) 50)
              (new posn% 0 50))
(check-expect (send (new posn% 0 0) move-toward (new posn% 101 100) 50)
              (new posn% 50 0))
  
      
(check-expect (send (new player% (new posn% 0 0)) move-toward (new mouse% 0 0))
              (new player% (new posn% 0 0)))
(check-expect (send (new player% (new posn% 0 0)) move-toward (new mouse% 100 0))
              (new player% (new posn% PLAYER-VELOCITY 0)))
(check-expect (send (new player% (new posn% 0 0)) move-toward (new mouse% 0 100))
              (new player% (new posn% 0 PLAYER-VELOCITY)))
(check-expect (send (new player% (new posn% 0 0)) move-toward (new mouse% 100 100))
              (new player% (new posn% PLAYER-VELOCITY 0)))
(check-expect (send (new player% (new posn% 0 0)) move-toward (new mouse% 100 101))
              (new player% (new posn% 0 PLAYER-VELOCITY)))
(check-expect (send (new player% (new posn% 0 0)) move-toward (new mouse% 101 100))
              (new player% (new posn% PLAYER-VELOCITY 0)))


(define-class mouse%
  (fields x y)
  
  ;; draw-on : Scene -> Scene
  ;; Draw this mouse on the scene
  (check-expect (send (new mouse% 10 20) draw-on (empty-scene 100 100))
                (place-image (square 5 "solid" "red")
                             10
                             20
                             (empty-scene 100 100)))  
  (define (draw-on scn)
    (place-image (square 5 "solid" "red")
                 (send this x)
                 (send this y)
                 scn)))

;; A ZombieApocalypse (ZA) is a (new za% Zombies Player Mouse)
;; implements
;; - on-tick : -> ZA
;; - to-draw : -> Scene
;; - on-mouse : MouseEvent Number Number -> ZA
;; - stop-when : -> Boolean

(define WIDTH 200)
(define HEIGHT 200)
(define-class za%
  (fields zombies player mouse)
  ;; on-tick : -> ZA
  (define (on-tick) 
    (new za% 
         (send (send (send this zombies) kill-any-touching) move-toward (send this player))
         (send (send this player) move-toward (send this mouse))
         (send this mouse)))
  
  ;; to-draw : -> Scene
  (define (to-draw)
    (send (send this zombies) draw-on
          (send (send this player) draw-on
                (send (send this mouse) draw-on
                      (empty-scene WIDTH HEIGHT)))))
  
  ;; on-mouse : Number Number MouseEvent -> ZA
  (define (on-mouse mx my me)
    (new za%
         (send this zombies)
         (send this player)
         (new mouse% mx my)))
  
  ;; stop-when : -> Boolean
  (define (stop-when) 
    (send (send this zombies) any-eating-player-brains? 
          (send this player))))


(define world0
  (new za% 
       (new conz% 
            (new undead% (new posn% 60 60))
            (new conz%
                 (new undead% (new posn% 100 100))
                 (new emptyz%)))
       (new player% (new posn% 0 0))
       (new mouse% 0 0)))
       
                
)