#lang racket
;; Zombie SERVER

;; Play the classic game of Zombie Brains!
;; All zombies move towards the *closest* living player.  Zombies collision 
;; cause flesh heaps that are deadly to other zombies (and you!).  Teleport
;; as a last resort!

;; Based on Robot!, p. 234 of Barski's Land of Lisp.
(provide (all-defined-out))
(require 2htdp/universe)
(require "shared.rkt")

(struct game (player-map zombies) #:transparent)
;; A Game is a (game PlayerMap [Setof Zombie])
;; A PlayerMap is a [Setof (List IWorld Player)]
;; Interp: relates IWorld's to their player's position.

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
      (make-bundle (game-tick us)
                   (broadcast us)
                   empty))
    1/5)
   (on-new
    (λ (us iw)
      (empty-bundle (game (set-add (game-player-map us)
                                   (list iw 0))
                          (game-zombies us)))))
   (on-msg 
    (λ (us iw m)
      (empty-bundle (game (cond [(teleport? m)
                                 (world-teleport iw 
                                                 (teleport-posn m) 
                                                 (game-player-map us))]                            
                                [(toward? m)
                                 (world-toward iw 
                                               (toward-posn m) 
                                               (game-player-map us))])
                          (game-zombies us)))))))

;; Game -> Game
;; Advance the game one tick.
(define (game-tick g)
  (let ((ps (players (game-player-map g))))
    (game (chomp (game-zombies g) (game-player-map g))
          (move (junk ps
                      (game-zombies g))
                ps))))

;; Zombies PlayerMap -> PlayerMap
;; All Zombies chomp: if they eat a player, the player dies.
(define (chomp zs pm)
  (player-map-set (λ (iw p)
                    (if (and (touches? p zs)
                             (not (dead? p)))
                        (list iw (dead p))
                        (list iw p)))
                  pm))

;; [Setof Zombie] [Setof Player] -> [Setof Zombie]
;; Move all the zombies toward the closest living player.
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

;; Game -> [Listof Mail]
;; Mail player and zombie coordinates to all the worlds.
(define (broadcast g)
  (local [(define zs (set-map-list (λ (z) z) (game-zombies g)))
          (define (notify iw p)
            (make-mail iw
                       (list p
                             (opponent-posns iw
                                             (game-player-map g))
                             zs)))]
    (player-map-list notify
                     (game-player-map g))))

;; IWorld PlayerMap -> [Listof Player]
;; Get positions for all opponents on map of given world's player.
(define (opponent-posns iw pm)
  (player-map-fold (λ (iw0 p ls)
                     (cond [(iworld=? iw iw0) ls]
                           [else (cons p ls)]))
                   empty
                   pm))

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

;; PlayerMap -> [Setof Player]
;; Get positions of all players on map.
(define (players pm)
  (player-map-set (λ (iw p) p) 
                  pm))

;; [IWorld Player -> X] X PlayerMap -> X
(define (player-map-fold f b pm)
  (set-fold (λ (x r)
              (f (first x) (second x) r))
            b
            pm))

;; [IWorld Player -> X] PlayerMap -> [Setof X]
(define (player-map-set f pm)
  (set-map (λ (x) (f (first x) (second x))) 
           pm))

;; [IWorld Player -> X] PlayerMap -> [Listof X]
(define (player-map-list f pm)
  (set-fold (λ (x ls) (cons (f (first x) (second x)) ls))
            empty
            pm))

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
  (cond [(set-empty? ps) u]
        [else
         (set-fold (λ (p q)
                     (if q
                         (if (< (taxi-dist p u) 
                                (taxi-dist q u))
                             p
                             q)
                         p))
                   false
                   ps)]))
  
;; Posn -> Posn -> Boolean
;; Are the points "touching"?
(define ((touching? p1) p2)
  (<= (taxi-dist p1 p2) 
      (/ *cell-size* 2)))

;; IWorld [Player -> Player] PlayerMap -> PlayerMap
(define (update-world-posn iw f pm)
  (player-map-set (λ (iw0 p)
                    (list iw0 
                          (cond [(iworld=? iw iw0) (f p)]
                                [else p])))
                  pm))

;; Move p1 toward p2
(define (move-toward p1 p2)
  (+ p1 ((minimize taxi-dist) p1 p2)))

;; Game -> Bundle
(define (empty-bundle g)
  (make-bundle g empty empty))

