#lang class0
;; ==========================================================
;; Play the classic game of Zombie Brains!

;; All zombies move towards the player.  The player moves 
;; toward the mouse. Zombies collision cause flesh heaps 
;; that are deadly to other zombies and the player.  
;; Randomly teleport via mouse click as a last resort!

;; Based on Robot!, p. 234 of Barski's Land of Lisp.
(require 2htdp/image)
(require class0/universe)

(define CELL-SIZE 20)
(define WIDTH 400)
(define HEIGHT 400)
(define MT-SCENE (empty-scene WIDTH HEIGHT))

;; FIXME: Inconsistent state -- moving toward more OO-design.

;; FIXME: change to 
;; A World is a (new world% Dot LoDot LoDot Posn).
;; Interp: player, living zombies, dead zombies, mouse position.

;; A World is a (new world% Posn [Listof Posn] [Listof Posn] Posn).
;; A Posn is a Int + (* +i Int).

(define-class world%
  (fields player live dead mouse)
  
  (define/public (name)
    "Zombie Attack!")
  
  (define/public (on-tick)
    (send (junk-it) move))
  
  (define/public (tick-rate)
    1/10)    
  
  ;; -> Scene
  (define/public (to-draw)
    (send (field live) draw-on "red"
          (send (field dead) draw-on "gray"
                (send (field player) draw-on "green"
                      MT-SCENE))))
  
  ;; Int Int Mouse -> World
  (define/public (on-mouse x y m)
    (cond [(mouse=? "button-down" m)
           (new world%
                (new dot% x y)   ;; FIXME: make random
                (field live)
                (field dead) 
                (new dot% x y))]
          [(mouse=? "move" m)
           (new world%
                (field player)
                (field live)
                (field dead)
                (new dot% x y))]
          [else this]))
  
  (define/public (stop-when)
    (game-over?))
  
  ;; -> Boolean
  ;; Does the player touch any zombies?
  (define/public (game-over?)
    (or (send (field dead) touching? (field player))
        (send (field live) touching? (field player))))
  
  ;; -> World
  ;; Move all the zombies toward the player.
  (define/public (move)
    (new world%
         (send (field player) move-toward 5
               (field mouse))
         (send (field live) move-toward 1
               (field player))
         (field dead)
         (field mouse)))
  
  ;; -> World
  ;; Junk all zombies that touch other zombies or junk.
  (define/public (junk-it)      
    (let ((res (send (field live) kill (field dead))))
      (new world% 
           (field player)
           (r-live res)
           (r-dead res)
           (field mouse)))))

(define-struct r (live dead))

;; Posn Posn -> Nat
;; Distance in taxicab geometry (domain knowledge).
(define (dist p1 p2)
  (+ (abs (- (posn-x p1)
             (posn-x p2)))
     (abs (- (posn-y p1)
             (posn-y p2)))))

;; FIXME: this contract is broken since the function returns
;; x * Dir, which is not a Dir by the current definition unless
;; x is in {-1,0,1}.

;; Nat Posn Posn -> Dir
;; Compute a direction that minimizes the distance between from and to.
(define (min-taxi x from to)
  ;; Dir Dir -> Dir
  (local [(define (select-shorter-dir d1 d2)
            (if (< (dist (posn+ from d1) to)
                   (dist (posn+ from d2) to))
                d1
                d2))]
    (foldl (λ (d sd) (select-shorter-dir sd (make-posn (* x (posn-x d)) (* x (posn-y d)))))
           (make-posn 0 0)
           DIRS)))

;; A Dir (Direction) is one of:
(define DIRS
  (list (make-posn -1 -1)
        (make-posn -1  0)
        (make-posn -1 +1)
        (make-posn  0 -1)
        (make-posn  0  0)
        (make-posn  0 +1)
        (make-posn +1 -1)
        (make-posn +1  0)
        (make-posn +1 +1)))


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

;; LoDot -> (make-r LoDot LoDot)
;; Kill any live zombies in this list and move to dead list.

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
             (let ((res (send (field rest) kill dead)))
               (make-r (new cons% z (r-live res))
                       (r-dead res)))]))))


;; ==========================================================
;; A Dot is a (new dot% Int Int).
(define-class dot%
  (fields x y)
  
  ;; Is this dot touching the given dot?
  ;; Dot -> Boolean
  (define/public (touching? d)
    (<= (dist d) 
        (/ CELL-SIZE 2)))
  
  ;; Move this dot n units toward the given dot.
  ;; Nat Nat -> Dot
  ;; FIXME: temporary hack
  (define/public (move-toward n d)
    (let ((new-posn            
           (posn+ (make-posn (field x)
                             (field y))
                  (min-taxi n 
                            (make-posn (field x)
                                       (field y))
                            (make-posn (send d x)
                                       (send d y))))))
      (new dot% 
           (posn-x new-posn)
           (posn-y new-posn))))
  
  ;; Draw this dot with the given color on the scene.
  ;; Color Scene -> Scene
  (define/public (draw-on c scn)
    (place-image (circle (/ CELL-SIZE 2) "solid" c)
                 (field x)
                 (field y)
                 scn))
  
  ;; Compute taxi distance from this dot to the given dot.
  ;; Dot -> Nat
  (define/public (dist d)
    (+ (abs (- (field x)
               (send d x)))
       (abs (- (field y)
               (send d y))))))

;; Posn Posn -> Posn
(define (posn+ p1 p2) 
  (make-posn (+ (posn-x p1) (posn-x p2))
             (+ (posn-y p1) (posn-y p2))))

;; Nat (-> Nat Dot) -> LoDot
;; Like build-list for LoDot.
(define (build-lodot n f)
  (local [(define (loop i)
            (cond [(= i n) (new empty%)]
                  [else
                   (new cons% 
                        (f i)
                        (loop (add1 i)))]))]
    (loop 0)))


;; Run program, run!
(big-bang
 (new world%
      (new dot% 0 0)
      (build-lodot (+ 10 (random 20))
                   (λ (_)
                     (new dot% 
                          (random WIDTH)
                          (random HEIGHT))))
      (new empty%)
      (new dot% 0 0)))