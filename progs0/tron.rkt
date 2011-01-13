#lang racket
(require 2htdp/universe)
(require 2htdp/image)

;; Two player distributed Tron.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shared

;; A Trail is a (cons Posn [Listof Posn]).
;; A Posn is a [List Number Number].
(define (posn x y) (list x y))
(define posn-x first)
(define posn-y second)

(define GRID-WIDTH 30)
(define GRID-HEIGHT 30)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server

;; A Universe is one of:
;; - #f
;; - IWorld
;; - (game Cycle Cycle)
;; Interp: #f = No players, IWorld = waiting player, game = two player game

;; A Cycle is a (cycle IWorld Dir Trail)
;; Interp: owner, direction, and trail.

;; A Dir is one of: "up", "down", "left", "right"

(struct cycle (owner dir trail) #:transparent)
(struct game (p1 p2) #:transparent)

(define (dir? x)
  (member x '("up" "down" "left" "right")))

;; Game -> Game
(define (game-tick g)
  (game (cycle-tick (game-p1 g))
        (cycle-tick (game-p2 g))))

;; Cycle -> Cycle
(define (cycle-tick c)
  (cycle (cycle-owner c)
         (cycle-dir c)
         (trail-grow (cycle-trail c) 
                     (cycle-dir c))))

;; Trail Dir -> Trail
(define (trail-grow t d)
  (cons (cond [(string=? d "up") 
               (posn (posn-x (first t))
                     (add1 (posn-y (first t))))]
              [(string=? d "down")
               (posn (posn-x (first t))
                     (sub1 (posn-y (first t))))]
              [(string=? d "left")
               (posn (add1 (posn-x (first t)))
                     (posn-y (first t)))]
              [(string=? d "right")
               (posn (sub1 (posn-x (first t)))
                     (posn-y (first t)))])
        t))

;; Game IWorld Dir -> Game
(define (change-dir g iw d)
  (cond [(iworld=? iw (cycle-owner (game-p1 g)))
         (game (cycle iw d (cycle-trail (game-p1 g)))
               (game-p2 g))]
        [(iworld=? iw (cycle-owner (game-p2 g)))
         (game (game-p1 g)
               (cycle iw d (cycle-trail (game-p2 g))))]))

;; Game -> [Listof Mail]
(define (game-broadcast g)
  (list (make-mail (cycle-owner (game-p1 g))
                   (list (cycle-trail (game-p1 g))
                         (cycle-trail (game-p2 g))))
        (make-mail (cycle-owner (game-p2 g))
                   (list (cycle-trail (game-p2 g))
                         (cycle-trail (game-p1 g))))))

;; Game -> [Listof Mail]
(define (game-end g)
  (append (game-broadcast g)
          (cond [(die? (cycle-trail (game-p1 g))
                       (cycle-trail (game-p2 g)))
                 (cond [(die? (cycle-trail (game-p2 g))
                              (cycle-trail (game-p1 g)))
                        (end-draw (game-p1 g) 
                                  (game-p2 g))]
                       [else
                        (end-win (game-p1 g) 
                                 (game-p2 g))])]
                [else 
                 (end-win (game-p2 g) 
                          (game-p1 g))])))
         

;; Cycle Cycle -> [Listof Mail]
(define (end-draw p1 p2)
  (list (make-mail (cycle-owner p1)
                   'draw)
        (make-mail (cycle-owner p2)
                   'draw)))

;; Cycle Cycle -> [Listof Mail]
(define (end-win p1 p2)
  (list (make-mail (cycle-owner p1)
                   'win)
        (make-mail (cycle-owner p2)
                   'lose)))

;; IWorld IWorld -> Game
(define (init-game iw1 iw2)
  (game (cycle iw1 
               "left" 
               (list (posn (quotient GRID-WIDTH 3)
                           (quotient GRID-HEIGHT 2))))
        (cycle iw2 
               "right"
               (list (posn (* 2 (quotient GRID-WIDTH 3))
                           (quotient GRID-HEIGHT 2))))))

;; Trail Trail -> Boolean
(define (die? t1 t2)
  (or (out-of-bounds? t1)
      (hit-trail? t1 t2)))
                     

;; Trail -> Boolean
(define (out-of-bounds? t)
  (local [(define hd (first t))]
    (or (< (posn-x hd) 0)
        (> (posn-x hd) GRID-WIDTH)
        (< (posn-y hd) 0)
        (> (posn-y hd) GRID-HEIGHT))))

;; Trail Trail -> Boolean
(define (hit-trail? t1 t2)
  (member (first t1)
          (append (rest t1) t2)))

;; Game -> Boolean
(define (game-over? g)
  (local [(define t1 (cycle-trail (game-p1 g)))
          (define t2 (cycle-trail (game-p2 g)))]
    (or (die? t1 t2)
        (die? t2 t1))))

(define (serve)
  (universe #f 
            (state #t)
            (on-new 
             (λ (u iw)
               (cond [(false? u)
                      (make-bundle iw empty empty)]
                     [(iworld? u) 
                      (make-bundle (init-game u iw)
                                   empty
                                   empty)]
                     [else
                      (make-bundle u empty empty)])))
            
            
            (on-tick 
             (λ (u)
               (cond [(game? u)
                      (cond [(game-over? u)
                             (make-bundle u
                                          (game-end u)
                                          empty)]
                            [else
                             (make-bundle (game-tick u)
                                          (game-broadcast u)
                                          empty)])]
                     [else
                      (make-bundle u empty empty)]))
             1)
            
            (on-msg
             (λ (u iw msg)
               (cond [(and (game? u) (dir? msg))
                      (make-bundle (change-dir u iw msg) empty empty)]
                     [else
                      (make-bundle u empty empty)])))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Client

(define GRID-SIZE 10)
(define WIDTH (* GRID-WIDTH GRID-SIZE))
(define HEIGHT (* GRID-HEIGHT GRID-SIZE))

(define MT-SCENE (empty-scene WIDTH HEIGHT))
(define WAITING 
  (overlay (text "Waiting for Player 2" 20 "black")
           MT-SCENE))                              

;; Match -> Scene
(define (draw-match m)
  (local [(define (draw-player c trail scn)
            (foldl (λ (p scn)
                     (place-image (square GRID-SIZE "solid" c)
                                  (* (posn-x p) GRID-SIZE)
                                  (- HEIGHT (* (posn-y p) GRID-SIZE))
                                  scn))
                   scn
                   trail))]
    (draw-player "blue"
                 (second m)
                 (draw-player "orange" 
                              (first m) 
                              MT-SCENE))))
                        

;; A World is one of:
;; - #f
;; - Match
;; - Final

;; A Final is a (make-final Outcome Match)
(struct final (outcome last))

;; An Outcome is one of: 'win, 'lose, 'draw
   
;; A Msg is one of:
;; - Match
;; - Outcome

;; Any -> Boolean
(define (outcome? w)
  (member w '(win lose draw)))

;; Final -> Scene
(define (draw-final f)
  (overlay (text (case (final-outcome f)
                   [(win) "You Win!"]
                   [(lose) "Loser!"]
                   [(draw) "TIE"])
                 40
                 "black")
           (draw-match (final-last f))))

;; A Match is a (list Trail Trail).

(define (play)
  (big-bang #f
            (register LOCALHOST)          
            (to-draw (λ (w)
                       (cond [(false? w) WAITING]
                             [(final? w) (draw-final w)]
                             [else (draw-match w)])))          
            (on-key (λ (w ke)
                      (cond [(false? w) w]
                            [(final? w) w]
                            [(dir? ke)
                             (make-package w ke)]
                            [else w])))          
            (on-receive (λ (w msg) 
                          ;; Potential bug: doesn't show collision
                          (cond [(outcome? msg) (final msg w)]
                                [else msg])))))
                       
          
(launch-many-worlds (serve) (play) (play))
                                         
                           
                           
                           
                            
                     




