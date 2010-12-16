#lang class0
;; Play the classic game of Zombie Brains!
;; All zombies move towards the player.  Zombies collision cause flesh heaps
;; that are deadly to other zombies (and you!).  Teleport as a last resort!

;; Based on Robot!, p. 234 of Barski's Land of Lisp.
(require 2htdp/image)
(require class0/universe)
(require (only-in racket for*/fold in-range))

;; A World is a (world Posn [Listof Posn] [Listof Posn]).
;; A Posn is a Int + (* +i Int).

(define-class world%
  (fields player zombies junk mouse-posn)
  
  (define/public (name)
    "Zombie Attack!")
  
  (define/public (on-tick)
    (send (junk-it) move))
  
  (define/public (tick-rate)
    1/10)
  
  ;; -> Scene
  (define/public (to-draw)
    (foldr (posn+scn "gray")
           (foldr (posn+scn "red") 
                  ((posn+scn "green")
                   (field player)
                   (empty-scene (posn-x *dim*)
                                (posn-y *dim*)))
                  (field zombies))
           (field junk)))
  
  ;; Int Int Mouse -> World
  (define/public (on-mouse x y m)
    (cond [(mouse=? "button-down" m)
           (new world%
                [player (posn x y)]
                [zombies (field zombies)]
                [junk (field junk)]
                [mouse-posn (posn x y)])]
          [(mouse=? "move" m)
           (new world%
                [player (field player)]
                [zombies (field zombies)]
                [junk (field junk)]
                [mouse-posn (posn x y)])]
          [else this]))

  (define/public (stop-when)
    (game-over?))
  
  ;; -> Boolean
  ;; Does the player touch zombies or junk?
  (define/public (game-over?)
    (ormap (touching? (field player))
           (append (field junk)
                   (field zombies))))
  
  ;; -> World
  ;; Move all the zombies toward the player.
  (define/public (move)
    (let ((p (field player)))
      (new world%
           [player (+ (field player) 
                      (min-taxi 5
                                (field player)
                                (field mouse-posn)))]
           [zombies (map (λ (r) (+ r (min-taxi 1 r p)))
                         (field zombies))]
           [junk (field junk)]
           [mouse-posn (field mouse-posn)])))

  ;; -> World
  ;; Junk all zombies that touch other zombies or junk.
  (define/public (junk-it)
    ;; maybe-zs are zombies if they don't touch any zs-to-consider.
    (local [(define (seg zs-to-consider maybe-zs junk)
              (cond [(empty? zs-to-consider) 
                     (new world%
                          [player (field player)]
                          [zombies (reverse maybe-zs)]
                          [junk junk]
                          [mouse-posn (field mouse-posn)])]
                    [else
                     (let ((touching-first? (touching? (first zs-to-consider))))
                       (cond [(ormap touching-first? (append maybe-zs junk))
                              (seg (rest zs-to-consider)
                                   (filter (λ (x) (not (touching-first? x))) maybe-zs)
                                   (cons (first zs-to-consider)
                                         (append (filter touching-first? maybe-zs)
                                                 junk)))]
                             [else
                              (seg (rest zs-to-consider)
                                   (cons (first zs-to-consider)
                                         maybe-zs)
                                   junk)]))]))]
      (seg (field zombies) empty (field junk)))))

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
 (new world%
      [player (posn 0 0)]
      [mouse-posn (posn 0 0)]
      [zombies (build-list (+ 20 (random 20))
                           (λ (_)
                             (random-posn *dim*)))]
      [junk empty]))