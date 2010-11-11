#lang racket
;; Zombie SERVER

;; Play the classic game of Zombie Brains!
;; All zombies move towards the *closest* living player.  Zombies collision 
;; cause flesh heaps that are deadly to other zombies (and you!).  Teleport
;; as a last resort!

;; Future enhancements:
;; - Dead players come back to life as zombies after N ticks.

;; Based on Robot!, p. 234 of Barski's Land of Lisp.
(provide (all-defined-out))
(require 2htdp/universe)
(require test-engine/scheme-tests)
(require "shared.rkt")

;; BUG: (dead 0) = 0.
(check-expect (dead? (dead 0)) true)

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

(check-expect (move (set 0+0i) (set 5+0i)) (set 1+0i))
(check-expect (move (set 0+0i) (set 5+0i)) (set 1+0i))

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
                   #;
                   [(set-ormap (λ (v)
                                 (and ((touching? u) 
                                       (pos-abs v))
                                      (dead? v)))
                               ps)
                    (dead u)]
                   [else u]))
           zs))

(check-expect (junk (set) (set 5+5i 6+6i))
              (set (dead 5+5i) (dead 6+6i)))
(check-expect (junk (set) (set 5+5i (dead 6+6i)))
              (set (dead 5+5i) (dead 6+6i)))

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

;; IWorld Posn PlayerMap -> PlayerMap
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

(check-expect (players (set (list iworld1 5+5i)))
              (set 5+5i))

;; [IWorld Player X -> X] X PlayerMap -> X
(define (player-map-fold f b pm)
  (set-fold (λ (x r)
              (f (first x) (second x) r))
            b
            pm))

(check-expect (player-map-fold (λ (iw p s) (+ s p))
                               0
                               (set (list iworld1 5+5i)))
              5+5i)

;; [IWorld Player -> X] PlayerMap -> [Setof X]
(define (player-map-set f pm)
  (set-map (λ (x) (f (first x) (second x))) 
           pm))

(check-expect (player-map-set list (set (list iworld1 0)))
              (set (list iworld1 0)))

;; [IWorld Player -> X] PlayerMap -> [Listof X]
(define (player-map-list f pm)
  (set-fold (λ (x ls) (cons (f (first x) (second x)) ls))
            empty
            pm))

(check-expect (player-map-list list (set (list iworld1 0)))
              (list (list iworld1 0)))

;; Posn Posn -> Nat
;; Distance in taxicab geometry (domain knowledge).
(define (taxi-dist p1 p2)
  (pos-sum (pos-abs (- p1 p2))))

(check-expect (taxi-dist 0 3+4i) 7)

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

(check-expect ((minimize taxi-dist) 0 6+6i) 1+1i)

;; (struct dead (posn))
;; BUGGY on 0.
(define dead -)

;; Posn [Listof Zombie] -> Boolean
(define (touches? p zs)
  (set-ormap (compose (touching? p) pos-abs)
             zs))

(check-expect (touches? 0 (set)) false)
(check-expect (touches? 0 (set 0)) true)
(check-expect (touches? 3+4i (set (dead 3+4i))) true)

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

(check-expect (closest (set) 0) 0)
(check-expect (closest (set 5+5i) 0) 5+5i)
(check-expect (closest (set 5+5i 6+6i) 0) 5+5i)
  
;; Posn -> Posn -> Boolean
;; Are the points "touching"?
(define ((touching? p1) p2)
  (<= (taxi-dist p1 p2) 
      (/ *cell-size* 2)))

(check-expect ((touching? 0) 0) true)
(check-expect ((touching? 0) 1) true)
(check-expect ((touching? 0) *cell-size*) false)

;; IWorld [Player -> Player] PlayerMap -> PlayerMap
(define (update-world-posn iw f pm)
  (player-map-set (λ (iw0 p)
                    (list iw0 
                          (cond [(iworld=? iw iw0) (f p)]
                                [else p])))
                  pm))

(check-expect (update-world-posn iworld1 add1 (set (list iworld1 0)))
              (set (list iworld1 1)))

;; Posn Posn -> Posn
;; Move p1 toward p2
(define (move-toward p1 p2)
  (+ p1 ((minimize taxi-dist) p1 p2)))

(check-expect (move-toward 0 5+5i) 1+1i)
(check-expect (move-toward 0 0+5i) 0+1i)

;; Game -> Bundle
;; Make a bundle with no mail and no removes.
(define (empty-bundle g)
  (make-bundle g empty empty))

(check-expect (empty-bundle 5) (make-bundle 5 empty empty))

(test)