#lang class0

;; Rubric:

;; Features:
;; Data: player, zombies, flesh       -  20pt
;; Drawing                            -  10pt
;; Remember last seen mouse position  -  20pt
;; Player pursues last seen mouse     -  20pt
;; Zombies pursue player              -  20pt
;; Flesh pursues nothing              -  10pt
;; Colliding zombies die              -  20pt
;; Colliding player ends game         -  20pt
;; Clicking randomly teleports player -  20pt
;; ==========================================
;; Subtotal                           - 160pt

;; Deductions per function/method:
;; Bad contract                       -  -2pt
;; Bad purpose                        -  -1pt
;; Bad tests                          -  -2pt

;; Overall:
;; Style                              -  20pt
;; Data defns                         -  20pt
;; ==========================================
;; Subtotal                           -  40pt

;; Features                           - 160pt
;; Overall                            -  40pt
;; ==========================================
;; Total                              - 200pt

;; Comments:

;; Built-in lists are fair game for this problem.

;; General Guidelines:

;; If someone repeatedly makes the same mistake, eg:
;;   Self Complex -> Complex
;; instead of
;;   Complex -> Complex
;; treat it like they did this on only half of the problems.
;; So if Self Complex -> Complex is worth 1 out of 2 contract
;; points, you'd get 9+9/2 (= 13.5 ~ 14) points for contracts
;; on methods.

;; Style points are up to the grader's discretion, but should
;; follow the style guidelines on the web page.

;; (Let us know if you have further guidelines we should add.)

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
;; A World is a (new world% Dot LoDot LoDot Posn).
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

;; touching? : -> Boolean
;; Does the player touch any zombies?

;; move-toward : -> World
;; Move all the zombies toward the player.

;; kill : -> World
;; Kill all zombies that touch other live or dead zombies.

(define-class world%
  (fields player live dead mouse)
  
  (define/public (name) "Zombie Attack!")
    
  (define/public (tick-rate) 1/10)
  
  (define/public (on-tick)
    (send (kill) move))  
  
  (define/public (to-draw)
    (send (field live) draw-on "red"
          (send (field dead) draw-on "gray"
                (send (field player) draw-on "green"
                      MT-SCENE))))
  
  (define/public (on-mouse x y m)
    (cond [(mouse=? "button-down" m)
           (teleport)]
          [(mouse=? "move" m)
           (mouse-move x y)]
          [else this]))    

  (define/public (teleport)
    (new world%
         (new dot% 
              (random WIDTH)
              (random HEIGHT))
         (field live)
         (field dead) 
         (field mouse)))
  
  (define/public (mouse-move x y)
    (new world%
         (field player)
         (field live)
         (field dead)
         (new dot% x y)))
  
  (define/public (stop-when)
    (or (send (field dead) touching? (field player))
        (send (field live) touching? (field player))))
  

  (define/public (move)
    (new world%
         (send (field player) move-toward P-SPEED
               (field mouse))
         (send (field live) move-toward Z-SPEED
               (field player))
         (field dead)
         (field mouse)))
  
  (define/public (kill)      
    (local [(define res 
              (send (field live) kill (field dead)))]
      (new world% 
           (field player)
           (r-live res)
           (r-dead res)
           (field mouse)))))


;; ==========================================================
;; A LoDot is one of:
;; - (new empty%)
;; - (new cons% Dot LoDot)

;; draw-on : Color Scene -> Scene
;; Draw this list of dots with the given color on the scene.

;; touching? : Dot -> Boolean
;; Are any dots in this list touching the given dot?

;; move-toward : Nat Dot -> LoDot
;; Move all dots in this list n units toward the given dot.

;; kill : LoDot -> (make-r LoDot LoDot)
;; Kill any live zombies in this list and move to dead list.

(define-struct r (live dead))

(define-class empty%
  (define/public (draw-on c scn)
    scn)
  
  (define/public (touching? d)
    false)
  
  (define/public (move-toward n d)
    this)
  
  (define/public (kill dead)    
    (make-r this dead)))

(define-class cons%
  (fields first rest)
  
  (define/public (draw-on c scn)
    (send (field first) draw-on c
          (send (field rest) draw-on c scn)))
  
  (define/public (touching? d)
    (or (send (field first) touching? d)
        (send (field rest) touching? d)))
  
  (define/public (move-toward n d)
    (new cons% 
         (send (field first) move-toward n d)
         (send (field rest) move-toward n d))) 
  
  (define/public (kill dead)
    (local [(define z (field first))]
      (cond [(or (send (field rest) touching? z)
                 (send dead touching? z))
             (send (field rest) kill
                   (new cons% z dead))]
            [else
             (local [(define res
                       (send (field rest) kill dead))]
               (make-r (new cons% z (r-live res))
                       (r-dead res)))]))))


;; ==========================================================
;; A Dot is a (new dot% [0,WIDTH] [0,HEIGHT]).
;; Interp: a position on the board.
;; A Vec is a (new dot% Int Int).
;; Interp: a Euclidean vector (a direction with magnitude).

;; touching? : Dot -> Boolean
;; Is this dot touching the given dot?

;; move-toward : Nat Nat -> Dot
;; Move this dot n units toward the given dot.

;; draw-on : Color Scene -> Scene
;; Draw this dot with the given color on the scene.

;; dist : Dot -> Nat
;; Compute taxi distance from this dot to the given dot.

;; min-taxi : Nat Dot -> Delta
;; Compute delta minimizing distance from this dot and to.

;; plus : Vec -> Dot
;; Move this dot by the given vector.

(define-class dot%
  (fields x y)
  
  (define/public (touching? d)
    (<= (dist d) 1/2-CELL))  

  (define/public (move-toward n d)
    (plus (min-taxi n d)))
  
  (define/public (draw-on c scn)
    (place-image (circle 1/2-CELL "solid" c)
                 (field x)
                 (field y)
                 scn))
  
  (define/public (dist d)
    (+ (abs (- (field x)
               (send d x)))
       (abs (- (field y)
               (send d y)))))  

  (define/public (min-taxi n to)
    ;; Vec Vec -> Vec
    (local [(define (select-shorter-dir d1 d2)
              (cond [(< (send (plus d1) dist to)
                        (send (plus d2) dist to))
                     d1]
                    [else d2]))]
      (foldl (λ (d sd) 
               (select-shorter-dir sd
                                   (new dot% 
                                        (* n (send d x)) 
                                        (* n (send d y)))))
             (new dot% 0 0)
             DIRS)))
  
  (define/public (plus d)
    (new dot% 
         (+ (field x) (send d x))
         (+ (field y) (send d y)))))

(define DIRS
  (list (new dot% -1 -1)
        (new dot% -1  0)
        (new dot% -1 +1)
        (new dot%  0 -1)
        (new dot%  0  0)
        (new dot%  0 +1)
        (new dot% +1 -1)
        (new dot% +1  0)
        (new dot% +1 +1)))


;; ==========================================================
;; Helper constructor for LoDot.

;; Nat (-> Nat Dot) -> LoDot
;; Like build-list for LoDot.
(check-expect (build-lodot 0 (λ (i) (new dot% i i)))
              (new empty%))
(check-expect (build-lodot 2 (λ (i) (new dot% i i)))
              (new cons% 
                   (new dot% 0 0)
                   (new cons%
                        (new dot% 1 1)
                        (new empty%))))
(define (build-lodot n f)
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
      (new dot% (/ WIDTH 2) (/ HEIGHT 2))
      (build-lodot (+ 10 (random 20))
                   (λ (_)
                     (new dot% 
                          (random WIDTH)
                          (random HEIGHT))))
      (new empty%)
      (new dot% 0 0)))


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
