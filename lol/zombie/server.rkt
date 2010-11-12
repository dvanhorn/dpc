;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname server) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; ISL+λ
;; Zombie SERVER

;; Play the classic game of Zombie Brains!
;; All zombies move towards the *closest* living player.  Zombies collision 
;; cause flesh heaps that are deadly to other zombies (and you!).  Teleport
;; as a last resort!

;; Future enhancements:
;; - Dead players come back to life as zombies after N ticks.

;; Based on Robot!, p. 234 of Barski's Land of Lisp.
(require class0)
(provide (all-defined-out))
(require 2htdp/universe)
(require (only-in racket set set-add negate for*/fold in-range set-empty?))
(require "shared.rkt")

(define-struct game (player-map zombies))
;; A Game is a (game PlayerMap [Setof Zombie])
;; A PlayerMap is a [Setof (List IWorld Player)]
;; Interp: relates IWorld's to their player's position.

;; Nat -> Game
;; Serve a game with n zombies.
(define (serve n)
  (universe
   (make-game (set)
              (apply set (build-list n 
                                     (λ (_)
                                       (random-posn *dim*)))))
   (on-tick
    (λ (us)
      (make-bundle (game-tick us)
                   (broadcast us)
                   empty))
    1/10)
   (on-new
    (λ (us iw)
      (empty-bundle (make-game (set-add (game-player-map us)
                                        (list iw 0))
                               (game-zombies us)))))
   (on-msg 
    (λ (us iw m)
      (empty-bundle (make-game (cond [(teleport? m)
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
    (make-game (chomp (game-zombies g) (game-player-map g))
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

(check-expect (move (set 0) (set 5)) (set 1))
(check-expect (move (set 0) (set 5)) (set 1))
(check-expect (move (set (dead 0)) (set 5)) (set (dead 0)))

;; [Setof Player] [Setof Zombie] -> [Setof Zombie]
;; Junk all zombies that touch other zombies or junk or dead players.
(define (junk ps zs)
  (set-map (λ (u)
             (cond [(dead? u) u]
                   [(set-ormap (λ (v)
                                 (and ((touching? u) 
                                       (meat-pos v))
                                      (not (= u v))))
                               zs)
                    (dead u)]
                   #;
                   [(set-ormap (λ (v)
                                 (and ((touching? u) 
                                       (meat-pos v))
                                      (dead? v)))
                               ps)
                    (dead u)]
                   [else u]))
           zs))

(check-expect (junk (set) (set 5+5i))
              (set 5+5i))
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


(check-expect (broadcast (make-game (set) (set)))
              empty)
(check-expect (broadcast (make-game (set (list iworld1 0)) (set)))
              (list (make-mail iworld1
                               (list 0 empty empty))))
(check-expect (broadcast (make-game (set (list iworld1 0)) (set 30+40i)))
              (list (make-mail iworld1
                               (list 0 empty (list 30+40i)))))
                                     
                               

;; IWorld PlayerMap -> [Listof Player]
;; Get positions for all opponents on map of given world's player.
(define (opponent-posns iw pm)
  (player-map-fold (λ (iw0 p ls)
                     (cond [(iworld=? iw iw0) ls]
                           [else (cons p ls)]))
                   empty
                   pm))

(check-expect (opponent-posns iworld1 (set (list iworld1 0)
                                           (list iworld2 5)))
              (list 5))

;; IWorld Posn PlayerMap -> PlayerMap
(define (world-teleport iw p ws)
  (update-world-posn iw 
                     (λ (q) 
                       (cond [(dead? q) q]
                             [else p]))
                     ws))

(check-expect (world-teleport iworld1 7 (set (list iworld1 0)
                                             (list iworld2 5)))
              (set (list iworld1 7)
                   (list iworld2 5)))
(check-expect (world-teleport iworld1 7 (set (list iworld1 (dead 0))))
              (set (list iworld1 (dead 0))))

;; IWorld Posn PlayerMap -> PlayerMap
(define (world-toward iw p ws)
  (update-world-posn iw 
                     (λ (q) 
                       (cond [(dead? q) q]
                             [else (move-toward q p)]))
                     ws))

(check-expect (world-toward iworld1 7 (set (list iworld1 0)
                                           (list iworld2 5)))
              (set (list iworld1 1)
                   (list iworld2 5)))
(check-expect (world-toward iworld1 7 (set (list iworld1 (dead 0))))
              (set (list iworld1 (dead 0))))

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

;; Posn [Listof Zombie] -> Boolean
(define (touches? p zs)
  (set-ormap (compose (touching? p) meat-pos)
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
                     (cond [(false? q) p]
                           [(< (taxi-dist p u) 
                               (taxi-dist q u))
                            p]
                           [else q]))
                   false
                   ps)]))

(check-expect (closest (set) 0) 0)
(check-expect (closest (set 5+5i) 0) 5+5i)
(check-expect (closest (set 5+5i 6+6i) 0) 5+5i)
(check-expect (closest (set 5+5i 3+3i) 0) 3+3i)
  
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

(check-expect (update-world-posn iworld1 add1 (set (list iworld1 0)
                                                   (list iworld2 5)))
              (set (list iworld1 1)
                   (list iworld2 5)))

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
