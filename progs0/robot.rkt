#lang racket
;; Play the classic game of Robots Attack!
;; All robots move towards the player.  Robot collision cause junk heaps
;; that are deadly to other robots (and you!).  Teleport as a last resort!

;; Based on Robot!, p. 234 of Barski's Land of Lisp.
(require 2htdp/universe)
(require 2htdp/image)
(require (only-in racket for*/fold in-range))

;; A World is a (world Posn [Listof Posn] [Listof Posn]).
;; A Posn is a Int + (* +i Int).
(struct world (player robots junk mouse-posn) #:transparent)

;; World -> World
(define (tock w)
  (move (junk-it w)))

(define tick-rate 1/10)

;; World -> Scene
(define (render w)
  (foldr (posn+scn "gray")
         (foldr (posn+scn "red") 
                ((posn+scn "green")
                 (world-player w)
                 (empty-scene (posn-x *dim*)
                              (posn-y *dim*)))
                (world-robots w))
         (world-junk w)))

;; World Int Int Mouse -> World
(define (squeak w x y m)
  (cond [(mouse=? "button-down" m)
         (world (random-posn *dim*)
                (world-robots w)
                (world-junk w) 
                (posn x y))]
        [(mouse=? "move" m)
         (world (world-player w)
                (world-robots w)
                (world-junk w)
                (posn x y))]
        [else w]))

;; World -> Boolean
;; Does the player touch robots or junk?
(define (game-over? w)
  (ormap (touching? (world-player w))
         (append (world-junk w)
                 (world-robots w))))

;; World -> World
;; Move all the robots toward the player.
(define (move w)
  (let ((p (world-player w)))
    (world (+ (world-player w) 
              (min-taxi 5
                        (world-player w)
                        (world-mouse-posn w)))
           (map (λ (r) (+ r (min-taxi 1 r p)))
                (world-robots w))
           (world-junk w)
           (world-mouse-posn w))))

;; World -> World
;; Junk all robots that touch other robots or junk.
(define (junk-it w)
  ;; maybe-zs are robots if they don't touch any zs-to-consider.
  (local 
    [(define (seg zs-to-consider maybe-zs junk)
       (cond [(empty? zs-to-consider) 
              (world (world-player w)
                     (reverse maybe-zs)
                     junk
                     (world-mouse-posn w))]
             [else
              (let ((touching-first? (touching? (first zs-to-consider))))
                (cond [(ormap touching-first? (append maybe-zs junk))
                       (seg (rest zs-to-consider)
                            (filter (λ (x) (not (touching-first? x))) 
                                    maybe-zs)
                            (cons (first zs-to-consider)
                                  (append (filter touching-first? maybe-zs)
                                          junk)))]
                      [else
                       (seg (rest zs-to-consider)
                            (cons (first zs-to-consider)
                                  maybe-zs)
                            junk)]))]))]
    (seg (world-robots w)
         empty 
         (world-junk w))))

;; Posn Posn -> Nat
;; Distance in taxicab geometry (domain knowledge).
(define (taxi-dist p1 p2)
  (posn-sum (posn-abs (- p1 p2))))

;; [Posn -> Nat] -> Nat Posn Posn -> Dir
;; Compute a direction that minimizes the distance between from and to.
(define ((minimize dist) x from to)
  (define (select-shorter-dir d1 d2)
    (if (< (dist (+ from d1) to)
           (dist (+ from d2) to))
        d1
        d2))
  (for*/fold ([d 0])
    ([i (in-range -1 2)]
     [j (in-range -1 2)])
    (select-shorter-dir d (posn (* x i) (* x j)))))

;; Posn Posn -> Dir
(define min-taxi (minimize taxi-dist))

(define *cell-size* 20)

;; Posn -> Posn -> Boolean
;; Are the points "touching"?
(define (touching? p1) 
  (λ (p2)
    (<= (taxi-dist p1 p2) 
        (/ *cell-size* 2))))

;; Color -> Posn Scene -> Scene
;; Render the posn on the scene in the given color.
(define (posn+scn c) 
  (λ (p s)
    (place-image (circle (/ *cell-size* 2) "solid" c)
                 (posn-x p)
                 (posn-y p)
                 s)))   

;; Posn library
;; Nat Nat -> Posn
(define (posn x y)
  (+ x (* 0+1i y)))
(define posn-x real-part)
(define posn-y imag-part)
(define (posn-abs p) 
  (posn (abs (real-part p))
        (abs (imag-part p))))
(define (posn-sum p)
  (+ (posn-x p) (posn-y p)))
(define (random-posn p)
  (let ((r (random (* (real-part p) (imag-part p)))))
    (posn (quotient r (posn-y p))
          (remainder r (posn-y p)))))

(define *dim* (posn 400 400))

;; Run program, run!
(big-bang 
 (world (/ *dim* 2)
        (build-list (+ 10 (random 20))
                    (λ (_)
                      (random-posn *dim*)))
        empty
        (posn 0 0)) 
 (name "Robots Attack!")
 (on-tick tock tick-rate)
 (to-draw render)
 (on-mouse squeak)
 (stop-when game-over?))

