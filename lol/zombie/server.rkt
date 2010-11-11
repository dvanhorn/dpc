#lang racket
(provide (all-defined-out))
(require 2htdp/universe)
(require "shared.rkt")

(struct game (worlds zombies) #:transparent)
;; A Game is a:
;; - (game [Setof (List IWorld Player)] [Setof Zombie])

;; Nat -> Game
;; Serve a game with n zombies.
(define (serve n)
  (universe
   (game (set)
         (apply set (build-list n 
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
        (move (junk (for/set ([p (in-set (game-worlds g))]) (second p)) (game-zombies g))
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
  (let ((live-ps (set-filter (negate dead?) ps)))
    (set-map (λ (u) 
               (cond [(dead? u) u]
                     [else (move-toward u (closest live-ps u))]))
             zs)))

;; [Setof Player] [Setof Zombie] -> [Setof Zombie]
;; Junk all zombies that touch other zombies or junk or dead players.
(define (junk ps zs)
  (set-map (λ (u)
             (cond [(dead? u) u]
                   [(set-ormap (λ (v)
                                 (and ((touching? u) 
                                       (pos-abs v))
                                      (not (= u v))))
                               zs)
                    (dead u)]
                   [(set-ormap (λ (v)
                                 (and ((touching? u) 
                                       (pos-abs v))
                                      (dead? v)))
                               ps)
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
  (pos-sum (pos-abs (- p1 p2))))

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
    (select-shorter-dir d (pos i j))))

;; (struct dead (posn))
(define dead -)

;; Posn [Listof Zombie] -> Boolean
(define (touches? p zs)
  (set-ormap (compose (touching? p) pos-abs)
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



;; Move p1 toward p2
(define (move-toward p1 p2)
  (+ p1 ((minimize taxi-dist) p1 p2)))

;; Game -> Bundle
(define (empty-bundle g)
  (make-bundle g empty empty))

