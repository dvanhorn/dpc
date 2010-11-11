#lang racket
;; Play the classic game of Zombie Brains!
;; All zombies move towards the *closest* living player.  Zombies collision 
;; cause flesh heaps that are deadly to other zombies (and you!).  Teleport
;; as a last resort!

;; Based on Robot!, p. 234 of Barski's Land of Lisp.
(require 2htdp/universe)
(require 2htdp/image)
  
;; A Zombie is a Meat.
;; A Player is a Meat.
;; A Meat is one of:         ; interp:
;; - (+ (- Nat) (* +i Nat))  ; dead meat
;; - (+ (+ Nat) (* +i Nat))  ; live meat
;; A Posn is a (+ Nat (* +i Nat)).
;; A World is one of:
;; - (world Player [Setof Player] [Setof Zombie])
(struct world (player opponents zombies))

(define *dim* 400+400i)
(define *cell-size* 20)

;; World -> World
(define (play w)
  (big-bang w
            (register LOCALHOST)
            (on-draw draw)
            (on-mouse squeak)
            (on-receive 
             (λ (w m)               
               (world (first m)
                      (apply set (second m))
                      (apply set (third m)))))))

;; World -> Scene
(define (draw w)
  (set-fold (λ (o scn)
              ((posn+scn (if (dead? o) "black" "orange"))
               (posn-abs o)
               scn))
            (set-fold (λ (u scn)
                        ((posn+scn (if (dead? u) "gray" "red"))
                         (posn-abs u)
                         scn))
                      ((posn+scn (if (dead? (world-player w)) "purple" "green"))
                       (posn-abs (world-player w))
                       (empty-scene (posn-x *dim*)
                                    (posn-y *dim*)))
                      (world-zombies w))
            (world-opponents w)))

;; World Int Int Mouse -> World
(define (squeak w x y m)
  (make-package w
                (cond [(mouse=? "button-down" m)
                       `(teleport ,(posn x y))]
                      [else
                       `(toward ,(posn x y))])))

;; Color -> Posn Scene -> Scene
;; Render the posn on the scene in the given color.
(define ((posn+scn c) p s)
  (place-image (circle (/ *cell-size* 2) "solid" c)
               (posn-x p)
               (posn-y p)
               s)) 

(define dead? (compose negative? real-part))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Posn and Set libraries

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

;; Set library
(define (set-map f s)
  (for/set ([x (in-set s)]) (f x)))
(define (set-ormap f s)
  (for/or ([x (in-set s)]) (f x)))
(define (set-fold f b s)
  (for/fold ([r b])
    ([x (in-set s)])
    (f x r)))
(define (set-filter f s)
  (for/set ([x (in-set s)]
            #:when (f x))
           x))
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UNIVERSE

(struct game (worlds zombies) #:transparent)
;; A Game is a:
;; - (game [Setof (List IWorld Player)] [Setof Zombie])

;; Nat -> Game
;; Serve a game with n zombies.
(define (serve n)
  (universe
   (game (set)
         (apply set (build-list n #;(+ 20 (random 20))
                                (λ (_)
                                  (random-posn *dim*)))))   
   (on-tick
    (λ (us)
      (make-bundle (game-step us)
                   (broadcast us)
                   empty))
    1/5)
   (on-new
    (λ (us iw)
      (empty-bundle (game (set-add (game-worlds us)
                                   (list iw 0))
                          (game-zombies us)))))
   (on-msg 
    (λ (us iw m)
      (empty-bundle (game (cond [(teleport? m)
                                 (world-teleport iw 
                                                 (teleport-posn m) 
                                                 (game-worlds us))]                            
                                [(toward? m)
                                 (world-toward iw 
                                               (toward-posn m) 
                                               (game-worlds us))])
                          (game-zombies us)))))))

;; Game -> Game
(define (game-step g)
  (game (for/set ([x (in-set (game-worlds g))])
                 (if (and (touches? (second x) (game-zombies g))
                          (not (dead? (second x))))
                     (list (first x) (dead (second x)))
                     x))
        (move (junk (game-zombies g))
              (for/set ([x (in-set (game-worlds g))])
                       (second x)))))

;; Game -> [Listof Mail]
(define (broadcast g)
  (for/list ([x (in-set (game-worlds g))])
    (make-mail (first x)
               (list (second x)
                     (for/list ([y (in-set (game-worlds g))]
                                #:when (not (iworld=? (first y) (first x))))
                       (second y))
                     (set->list (game-zombies g))))))

;; [Setof Zombie] [Setof Player] -> [Setof Zombie]
;; Move all the zombies toward the closest player.
(define (move zs ps)
  (set-map (λ (u) 
             (cond [(dead? u) u]
                   [else
                    (move-toward u 
                                 (closest (set-filter (negate dead?) ps)
                                          u))]))
           zs))

;; [Setof Zombie] -> [Setof Zombie]
;; Junk all zombies that touch other zombies or junk.
(define (junk zs)
  (set-map (λ (u)
             (cond [(dead? u) u]
                   [(set-ormap (λ (v)
                                 (and ((touching? u) 
                                       (posn-abs v))
                                      (not (= u v))))
                               zs)
                    (dead u)]
                   [else u]))
           zs))

;; IWorld Posn [Setof (List IWorld Player)] -> [Setof (List IWorld Player)]
(define (world-teleport iw p ws)
  (update-world-posn iw 
                     (λ (q) 
                       (cond [(dead? q) q]
                             [else p]))
                     ws))

;; IWorld Posn [Setof (List IWorld Player)] -> [Setof (List IWorld Player)]
(define (world-toward iw p ws)
  (update-world-posn iw 
                     (λ (q) 
                       (cond [(dead? q) q]
                             [else (move-toward q p)]))
                     ws))

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

;; (struct dead (posn))
(define dead -)

;; Posn [Listof Zombie] -> Boolean
(define (touches? p zs)
  (set-ormap (compose (touching? p) posn-abs)
             zs))

;; [Setof Posn] Posn -> Posn
;; Pick the position closest to the given one.
(define (closest ps u)
  (if (set-empty? ps) u
      (for/fold ([q #f])
        ([p (in-set ps)])
        (if q
            (if (< (taxi-dist p u) 
                   (taxi-dist q u))
                p
                q)
            p))))
  
;; Posn -> Posn -> Boolean
;; Are the points "touching"?
(define ((touching? p1) p2)
  (<= (taxi-dist p1 p2) 
      (/ *cell-size* 2)))

;; IWorld [Player -> Player] [Setof (List IWorld Player)]
(define (update-world-posn iw f ws)
  (for/set ([x (in-set ws)])
           (if (iworld=? iw (first x))
               (list iw (f (second x)))
               x)))

(define (set->list s) (for/list ([x (in-set s)]) x))

(define (starts-with s)
  (λ (x)
    (and (cons? x)
         (eq? s (first x)))))
(define toward?   (starts-with 'toward))
(define teleport? (starts-with 'teleport))
(define toward-posn second)
(define teleport-posn second)

;; Move p1 toward p2
(define (move-toward p1 p2)
  (+ p1 ((minimize taxi-dist) p1 p2)))

;; Game -> Bundle
(define (empty-bundle g)
  (make-bundle g empty empty))


(define (demo n)
  ;; Run program, run!
  (launch-many-worlds
   (play (world 0 (set) (set)))
   (play (world 0 (set) (set)))
   (serve n)))
