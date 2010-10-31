#lang racket
;; Play the classic game of Zombie Brains!
;; All zombies move towards the player.  Zombies collision cause flesh heaps
;; that are deadly to other zombies (and you!).  Teleport as a last resort!

;; Based on Robot!, p. 234 of Barski's Land of Lisp.
(require 2htdp/universe)
(require 2htdp/image)

;; World -> World
(define (play w)
  (big-bang w
            (on-tick (compose junk move) 1/10)
            (on-draw draw)
            (on-mouse squeak)
            (stop-when game-over?)))

;; A World is a (world Posn [Listof Posn] [Listof Posn]).
;; A Posn is a Int + (* +i Int).
(struct world (player zombies junk) #:transparent)

(define *dim* 400+400i)

;; World -> Scene
(define (draw w)
  (foldr (posn+scn "gray")
         (foldr (posn+scn "red") 
                ((posn+scn "green")
                 (world-player w)
                 (empty-scene (posn-x *dim*)
                              (posn-y *dim*)))
                (world-zombies w))
         (world-junk w)))

;; World Int Int Mouse -> World
(define (squeak w x y m)
  (cond [(mouse=? "button-down" m)
         (world (posn x y)
                (world-zombies w)
                (world-junk w))]
        [else
          (world (+ (world-player w) 
                    (min-taxi (world-player w) 
                              (posn x y)))
                 (world-zombies w)
                 (world-junk w))]))

;; World -> Boolean
;; Does the player touch zombies or junk?
(define (game-over? w)
  (ormap (touching? (world-player w))
         (append (world-junk w)
                 (world-zombies w))))

;; World -> World
;; Move all the zombies toward the player.
(define (move w)
  (let ((p (world-player w)))
    (world p
           (map (λ (r) (+ r (min-taxi r p)))
                (world-zombies w))
           (world-junk w))))

;; World -> World
;; Junk all zombies that touch other zombies or junk.
(define (junk w)
  ;; maybe-zs are zombies if they don't touch any zs-to-consider.
  (define (seg zs-to-consider maybe-zs junk)
    (cond [(empty? zs-to-consider) 
           (world (world-player w) (reverse maybe-zs) junk)]
          [else
            (let ((touching-first? (touching? (first zs-to-consider))))
              (cond [(ormap touching-first? (append maybe-zs junk))
                     (seg (rest zs-to-consider)
                          (filter (negate touching-first?) maybe-zs)
                          (cons (first zs-to-consider)
                                (append (filter touching-first? maybe-zs)
                                        junk)))]
                    [else
                      (seg (rest zs-to-consider)
                           (cons (first zs-to-consider)
                                 maybe-zs)
                           junk)]))]))
  (seg (world-zombies w) empty (world-junk w)))

;; Posn Posn -> Nat
;; Distance in taxicab geometry (domain knowledge).
(define (taxi-dist p1 p2)
  (posn-sum (posn-abs (- p1 p2))))

;; [Posn -> Nat] -> Posn Posn -> Dir
;; Compute a direction that minimizes the distance between from and to.
(define ((minimize dist) from to)
  (define (select-shorter-dir d1 d2)
    (if (< (dist (+ from d1) to)
           (dist (+ from d2) to))
        d1
        d2))
  (for*/fold ([d 0])
    ([i (in-range -1 2)]
     [j (in-range -1 2)])
    (select-shorter-dir d (posn i j))))

;; Posn Posn -> Dir
(define min-taxi (minimize taxi-dist))

(define *cell-size* 20)

;; Posn -> Posn -> Boolean
;; Are the points "touching"?
(define ((touching? p1) p2)
  (<= (taxi-dist p1 p2) 
      (/ *cell-size* 2)))

;; Color -> Posn Scene -> Scene
;; Render the posn on the scene in the given color.
(define ((posn+scn c) p s)
  (place-image (circle (/ *cell-size* 2) "solid" c)
               (posn-x p)
               (posn-y p)
               s))    

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

;; Run program, run!
(play (world 0 
             (build-list (+ 20 (random 20))
                         (λ (_)
                           (random-posn *dim*)))
             empty))