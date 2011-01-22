#lang class0
;; ==========================================================
;; Play the classic game of Zombie Attack!

;; All zombies move toward the player.  The player moves 
;; toward the mouse. Zombies collision cause flesh heaps 
;; that are deadly to other zombies and the player.  
;; Randomly teleport via mouse click as a last resort!

;; Based on Robot!, p. 234 of Barski's Land of Lisp.
(require 2htdp/image)
(require class0/universe)

(define CELL 20)
(define 1/2-CELL (/ CELL 2))
(define WIDTH 400)
(define HEIGHT 400)
(define MT-SCENE (empty-scene WIDTH HEIGHT))
(define P-SPEED 5)
(define Z-SPEED 1)

;; ==========================================================
;; A World is a (new world% Player LoZombie Mouse).
;; Interp: player, list of living and dead zombies, mouse.

;; name : -> String
;; The name of the game.

;; tick-rate : -> Number
;; The animation rate for the game.

;; on-tick : -> World
;; Kill any zombies and move player and zombies.

;; to-draw : -> Scene
;; Draw the player and live and dead zombies.

;; on-mouse : Int Int MouseEvent -> World
;; Handle mouse events. 

;; teleport : -> World
;; Teleport to random location.

;; mouse-move : Int Int -> World
;; Record mouse movement.

;; kill : -> World
;; Kill all zombies that touch other zombies.

(define-class world%
  (fields player zombies mouse)
  
  (define/public (name) "Zombie Attack!")
    
  (define/public (tick-rate) 1/10)
  
  (define/public (on-tick)
    (send (kill) move))  
  
  (define/public (to-draw)
    (send (field zombies) draw-on
          (send (field player) draw-on
                MT-SCENE)))
  
  (define/public (on-mouse x y m)
    (cond [(mouse=? "button-down" m)
           (teleport)]
          [(mouse=? "move" m)
           (mouse-move x y)]
          [else this]))    

  (define/public (teleport)
    (new world%
         (new player% 
              (random WIDTH)
              (random HEIGHT))
         (field zombies)
         (field mouse)))
  
  (define/public (mouse-move x y)
    (new world%
         (field player)
         (field zombies)
         (new mouse% x y)))
  
  (define/public (stop-when)
    (send (field zombies) touching? (field player)))  

  (define/public (move)
    (new world%
         (send (field player) move-toward (field mouse))
         (send (field zombies) move-toward (field player))
         (field mouse)))
  
  (define/public (kill)      
    (new world%
         (field player)
         (send (field zombies) kill)
         (field mouse))))


;; ==========================================================
;; A Player is a (new player% [0,WIDTH] [0,HEIGHT]).

;; draw-on : Scene -> Scene
;; plus : Vec -> Player
;; move-toward : Mouse -> Player
(define-class player%
  (fields x y)
  
  (define/public (draw-on scn)
    (place-image (circle 1/2-CELL "solid" "green")
                 (field x)
                 (field y)
                 scn))
  
  (define/public (move-toward mouse)
    (plus (min-taxi this P-SPEED mouse)))
  
  (define/public (plus v)
    (new player%
         (+ (field x) (send v x))
         (+ (field y) (send v y)))))


;; ==========================================================
;; Vec is a (new vec% Int Int).
(define-class vec% (fields x y))


;; ==========================================================
;; A Mouse is a (new mouse% Int Int).
(define-class mouse% (fields x y))


;; ==========================================================
;; A Zombie is one of:
;; - LiveZombie 
;; - DeadZombie

;; move-toward : Player -> Zombie
;; touching? : [U Player Zombie] -> Boolean
;; draw-on : Scene -> Scene
;; kill : -> DeadZombie

;; A DeadZombie is a (new dead-zombie% [0,WIDTH] [0,HEIGHT]).
;; A LiveZombie is a (new live-zombie% [0,WIDTH] [0,HEIGHT]).

;; plus : Vec -> LiveZombie

(define-class live-zombie% 
  (fields x y)
  
  (define/public (move-toward p)
    (plus (min-taxi this Z-SPEED p)))
  
  (define/public (touching? p)
    (zombie-touching? this p))
  
  (define/public (draw-on scn)
    (place-image (circle 1/2-CELL "solid" "red")
                 (field x)
                 (field y)
                 scn))
  
  (define/public (kill)
    (new dead-zombie% (field x) (field y)))
  
  (define/public (plus v)
    (new live-zombie% 
         (+ (field x) (send v x))
         (+ (field y) (send v y)))))
    
(define-class dead-zombie% 
  (fields x y)
  (define/public (move-toward p)
    this)
  
  (define/public (touching? p)
    (zombie-touching? this p))
  
  (define/public (draw-on scn)
    (place-image (circle 1/2-CELL "solid" "gray")
                 (field x)
                 (field y)
                 scn))
  
  (define/public (kill)
    (new dead-zombie% (field x) (field y))))


;; ==========================================================
;; A LoZombie is one:
;; - (new empty%)
;; - (new cons% Zombie LoZombie)

;; draw-on : Scene -> Scene
;; Draw this list of zombies on the scene.

;; touching? : [U Player Zombie] -> Boolean
;; Are any of these zombies touching the player or zombie?

;; move-toward : Player -> LoDot
;; Move all zombies toward the player.

;; kill : -> LoZombie
;; Kill any zombies that touch others.

(define-struct r (live dead))

(define-class empty%
  (define/public (draw-on scn)
    scn)
  
  (define/public (touching? pz)
    false)
  
  (define/public (move-toward p)
    this)
  
  (define/public (kill)    
    this)
  
  (define/public (kill/acc seen)
    this))

(define-class cons%
  (fields first rest)
  
  (define/public (draw-on scn)
    (send (field first) draw-on
          (send (field rest) draw-on scn)))
  
  (define/public (touching? pz)
    (or (send (field first) touching? pz)
        (send (field rest) touching? pz)))
  
  (define/public (move-toward p)
     (new cons% 
         (send (field first) move-toward p)
         (send (field rest) move-toward p)))
  
  (define/public (kill) 
    (kill/acc (new empty%)))
  
  (define/public (kill/acc seen)
    ;; does first touch anything in rest or seen?
    (cond [(or (send seen touching? (field first))
               (send (field rest) touching? (field first)))
           (new cons%
                (send (field first) kill)
                (send (field rest) kill/acc
                      (new cons% (field first) seen)))]          
          [else
           (new cons%
                (field first)
                (send (field rest) kill/acc
                      (new cons% (field first) seen)))])))

;; ==========================================================
;; Functional abstractions

(define (dist p1 p2)
  (+ (abs (- (send p1 x)
             (send p2 x)))
     (abs (- (send p1 y)
             (send p2 y)))))

;; Zombie [U Player Zombie] -> Boolean
(define (zombie-touching? z p)
  (<= (dist z p) 1/2-CELL))

;; [U Player LiveZombie] Nat [U Player Mouse] -> Vec
(define (min-taxi from n to)
  ;; Vec Vec -> Vec
  (local [(define (select-shorter-dir d1 d2)
            (cond [(< (dist (send from plus d1) to)
                      (dist (send from plus d2) to))
                   d1]
                  [else d2]))]
    (foldl (λ (d sd) 
             (select-shorter-dir sd
                                 (new vec% 
                                      (* n (send d x)) 
                                      (* n (send d y)))))
           (new vec% 0 0)
           DIRS)))

(define DIRS
  (list (new vec% -1 -1)
        (new vec% -1  0)
        (new vec% -1 +1)
        (new vec%  0 -1)
        (new vec%  0  0)
        (new vec%  0 +1)
        (new vec% +1 -1)
        (new vec% +1  0)
        (new vec% +1 +1)))

;; ==========================================================
;; Helper constructor for LoDot.

;; Nat (-> Nat Dot) -> LoZ
;; Like build-list for LoZ.
(check-expect (build-loz 0 (λ (i) (new live-zombie% i i)))
              (new empty%))
(check-expect (build-loz 2 (λ (i) (new live-zombie% i i)))
              (new cons% 
                   (new live-zombie% 0 0)
                   (new cons%
                        (new live-zombie% 1 1)
                        (new empty%))))
(define (build-loz n f)
  (local [(define (loop i)
            (cond [(= i n) (new empty%)]
                  [else
                   (new cons% 
                        (f i)
                        (loop (add1 i)))]))]
    (loop 0)))


;; ==========================================================
;; Run program, run!
(big-bang
 (new world%
      (new player% (/ WIDTH 2) (/ HEIGHT 2))
      (build-loz (+ 10 (random 20))
                 (λ (_)
                   (new live-zombie% 
                        (random WIDTH)
                        (random HEIGHT))))
      (new mouse% 0 0)))

#|
;; ==========================================================
;; Test cases

(define mt (new empty%))
(define d0 (new dot% 0 0))
(define d1 (new dot% 0 30))
(define l0 (new cons% d0 mt))
(define l1 (new cons% d1 mt))
(define w0 (new world% d0 mt mt d0))
(define w1 (new world% d0 l0 mt d0))
(define w2 (new world% d0 mt l0 d0))
(define w3 (new world% d0 mt mt d1))
(define w4 (new world% d0 l1 l1 d1))
(define w5
  (new world% 
       (new dot% 0 P-SPEED)
       (new cons% (new dot% 0 (- 30 Z-SPEED)) mt)
       l1
       d1))
(define w6 (new world% d0 mt (new cons% d1 l1) d1))
(define w7 (send w0 teleport))

;; World tests
;; ===========

;; on-mouse
(check-expect (send w0 on-mouse 0 30 "drag") w0)
(check-expect (send w0 on-mouse 0 30 "move") w3)
;; teleport
(check-range (send (send w7 player) x) 0 WIDTH)
(check-range (send (send w7 player) y) 0 HEIGHT)
(check-expect (send w7 live) mt)
(check-expect (send w7 dead) mt)
(check-expect (send w7 mouse) d0)
;; mouse-move
(check-expect (send w0 mouse-move 0 30)
              (new world% d0 mt mt d1))
;; stop-when
(check-expect (send w0 stop-when) false)
(check-expect (send w1 stop-when) true)
(check-expect (send w2 stop-when) true)
;; move
(check-expect (send w4 move) w5)
;; kill
(check-expect (send w4 kill) w6)

;; LoDot tests
;; ===========

;; draw-on
(check-expect (send mt draw-on "red" MT-SCENE) MT-SCENE)
(check-expect (send l1 draw-on "red" MT-SCENE)
              (send d1 draw-on "red" MT-SCENE))
;; touching?
(check-expect (send mt touching? d0) false)
(check-expect (send l0 touching? d0) true)
(check-expect (send l0 touching? d1) false)
;; move-toward
(check-expect (send mt move-toward 5 d0) mt)
(check-expect (send l0 move-toward 5 d0) l0)
(check-expect (send l1 move-toward 5 d0)
              (new cons% (new dot% 0 25) mt))                
;; kill
(check-expect (send mt kill mt)
              (make-r mt mt))
(check-expect (send mt kill l1)
              (make-r mt l1))
(check-expect (send l1 kill mt)
              (make-r l1 mt))
(check-expect (send l1 kill l1)
              (make-r mt (new cons% d1 l1)))

;; Dot tests
;; =========

;; touching?
(check-expect (send d0 touching? d0) true)
(check-expect (send d0 touching? d1) false)
;; move-toward
(check-expect (send d0 move-toward 5 d0) d0)
(check-expect (send d1 move-toward 5 d0)
              (new dot% 0 25))
;; draw-on
(check-expect (send d0 draw-on "red" MT-SCENE)
              (place-image (circle 1/2-CELL "solid" "red")
                           0 0
                           MT-SCENE))
;; dist
(check-expect (send d0 dist d1) 30)
;; min-taxi
(check-expect (send d0 min-taxi 5 d0) d0)
(check-expect (send d0 min-taxi 5 d1) (new dot% 0 5))
;; plus
(check-expect (send d0 plus d0) d0)
(check-expect (send d0 plus d1) d1)
(check-expect (send d1 plus d1) 
              (new dot% 0 60))
|#