#lang class2
(require class1/universe)
(require 2htdp/image)

;; Two player distributed Tron.

;; -> Universe World World
;; Play a game locally.
(define (play-local)
  (launch-many-worlds (serve)
                      (play LOCALHOST)
                      (play LOCALHOST)))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shared

;; A Trail is a (cons Posn [Listof Posn]).
;; A Posn is a [List Number Number].
(define (posn x y) (list x y))
(define posn-x first)
(define posn-y second)

(define GRID-WIDTH 60)
(define GRID-HEIGHT 60)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server

;; A Universe is one of:   Interp:
;; - (init%)               - No players
;; - (wait% IWorld)        - One player, waiting on second
;; - (game% Cycle Cycle)   - two player game

(define (serve)
  (universe (init%)))

(define-class init%
  (define/public (tick-rate) 1/2)
  
  ;; IWorld -> Universe
  ;; Wait for second player.
  (define/public (on-new iw)
    (make-bundle (wait% iw) empty empty))
  
  ;; -> Universe
  ;; Continue to wait.
  (define/public (on-tick)
    (make-bundle this empty empty))
  
  ;; IWorld SExpr -> Universe
  ;; Ignore messages, continue to wait.
  (define/public (on-msg iw msg)
    (make-bundle this empty empty)))

(define-class wait%
  (fields w)
  
  ;; IWorld -> Universe
  ;; Let the game begin!
  (define/public (on-new iw)
    (make-bundle (init-game (field w) iw) empty empty))
  
  ;; -> Universe
  ;; Continue to wait.
  (define/public (on-tick)
    (make-bundle this empty empty))
  
  ;; IWorld SExpr -> Universe
  ;; Ignore messages, continue to wait.
  (define/public (on-msg iw msg)
    (make-bundle this empty empty)))

(define-class game% 
  (fields p1 p2)
  
  ;; IWorld -> Universe
  ;; Ignore new worlds.
  (define/public (on-new iw)
    (make-bundle this empty empty))
  
  ;; -> Universe
  ;; Advance this universe one tick.
  (define/public (on-tick)
    (cond [(game-over?)
           (make-bundle this (end) empty)]
          [else
           (make-bundle (tick) (broadcast) empty)]))
  
  ;; IWorld SExpr -> Universe
  ;; Change direction if appropriate.
  (define/public (on-msg iw msg)
    (cond [(dir? msg)
           (make-bundle (change-dir iw msg) empty empty)]
          [else
           (make-bundle this empty empty)]))

  ;; -> Game
  ;; Advance this game one tick.
  (define/public (tick)
    (game% ((field p1) . tick)
           ((field p2) . tick)))
  
  ;; IWorld Dir -> Game
  ;; Change the direction of the given iworld's cycle.
  (define/public (change-dir iw d)
    (cond [(iworld=? iw ((field p1) . owner))
           (game% (cycle% iw d ((field p1) . trail))
                  (field p2))]
          [(iworld=? iw ((field p2) . owner))
           (game% (field p1)
                  (cycle% iw d ((field p2) . trail)))]))
  
  ;; -> [Listof Mail]
  ;; Broadcast state of game to players.
  (define/public (broadcast)
    (list (make-mail ((field p1) . owner)
                     (list ((field p1) . trail)
                           ((field p2) . trail)))
          (make-mail ((field p2) . owner)
                     (list ((field p2) . trail)
                           ((field p1) . trail)))))
  
  ;; -> Boolean
  ;; Is this game over?
  (define/public (game-over?)
    (local [(define t1 ((field p1) . trail))
            (define t2 ((field p2) . trail))]
      (or (die? t1 t2)
          (die? t2 t1))))
  
  ;; -> [Listof Mail]
  ;; Construct mail notifying win, lose, or draw.
  (define/public (end)
    (append (broadcast)
            (cond [(die? ((field p1) . trail)
                         ((field p2) . trail))
                   (cond [(die? ((field p2) . trail)
                                ((field p1) . trail))
                          (end-draw)]
                         [else
                          (end-p2-win)])]
                  [else 
                   (end-p1-win)])))
  
  ;; -> [Listof Mail]
  ;; Construct mail notifying draw.
  (define/public (end-draw)
    (list ((field p1) . mail 'draw)
          ((field p2) . mail 'draw)))
  
  ;; -> [Listof Mail]
  ;; Construct mail notifying P1 win, P2 loss.
  (define/public (end-p1-win)
    (list ((field p1) . mail 'win)
          ((field p2) . mail 'lose)))
  
  ;; -> [Listof Mail]
  ;; Construct mail notifying P2 win, P1 loss.
  (define/public (end-p2-win)
    (list ((field p1) . mail 'lose)
          ((field p2) . mail 'win))))
    

;; A Cycle is a (cycle% IWorld Dir Trail)
;; Interp: owner, direction, and trail.

(define-class cycle%
  (fields owner dir trail)
  
  ;; Cycle -> Cycle
  ;; Advance this cycle one tick.
  (define/public (tick)
    (cycle% (field owner)
            (field dir)
            (trail-grow (field trail) 
                        (field dir))))
  
  ;; Msg -> Mail
  ;; Construct a mail message to the owner of this cycle.
  (define/public (mail msg)
    (make-mail (field owner) msg)))


;; A Dir is one of: "up", "down", "left", "right"

;; Any -> Boolean
(check-expect (dir? "a") false)
(check-expect (dir? "right") true)
(define (dir? x)
  (member x '("up" "down" "left" "right")))

;; Trail Dir -> Trail
(define (trail-grow t d)
  (cons (cond [(string=? d "up") 
               (posn (posn-x (first t))
                     (add1 (posn-y (first t))))]
              [(string=? d "down")
               (posn (posn-x (first t))
                     (sub1 (posn-y (first t))))]
              [(string=? d "left")
               (posn (sub1 (posn-x (first t)))
                     (posn-y (first t)))]
              [(string=? d "right")
               (posn (add1 (posn-x (first t)))
                     (posn-y (first t)))])
        t))

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

;; IWorld IWorld -> Game
;; Construct an initial game between the two given worlds.
(define (init-game iw1 iw2)
  (game% (cycle% iw1 
                 "right" 
                 (list (posn (quotient GRID-WIDTH 3)
                             (quotient GRID-HEIGHT 2))))
         (cycle% iw2 
                 "left"
                 (list (posn (* 2 (quotient GRID-WIDTH 3))
                             (quotient GRID-HEIGHT 2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Client

(define GRID-SIZE 10)
(define WIDTH (* GRID-WIDTH GRID-SIZE))
(define HEIGHT (* GRID-HEIGHT GRID-SIZE))

(define MT-SCENE (empty-scene WIDTH HEIGHT))
(define WAITING 
  (overlay (text "Waiting for Player 2" 20 "black")
           MT-SCENE))                              
                        
;; A World is one of:
;; - (mt-world%)
;; - (match% (list Trail Trail))
;; - (final% Outcome (list Trail Trail))

;; An Outcome is one of: 'win, 'lose, 'draw

;; Any -> Boolean
(check-expect (outcome? "fred") false)
(check-expect (outcome? 'draw) true)
(define (outcome? w)
  (member w '(win lose draw)))

;; IP -> World
(define (play ip)
  (big-bang (mt-world% ip)))

(define-class mt-world%
  (fields ip)
  (define/public (register) (field ip))
  (define/public (to-draw) WAITING)
  (define/public (on-key ke) this)
  (define/public (on-receive msg)   
    (match% msg)))

(define-class match%
  (fields m)
  (define/public (to-draw)
    (draw-match (field m)))
  (define/public (on-key ke)
    (cond [(dir? ke) (make-package this ke)]
          [else this]))
  (define/public (on-receive msg)
    (cond [(outcome? msg) (final% msg (field m))]
          [else (match% msg)])))

(define-class final%
  (fields outcome last)
  (define/public (to-draw)
    (overlay 
     (text (cond
             [(symbol=? (field outcome) 'win) "You Win!"]
             [(symbol=? (field outcome) 'lose) "Loser!"]
             [(symbol=? (field outcome) 'draw) "TIE"])
           40
           "black")
     (draw-match (field last))))
  
  (define/public (on-key ke)
    this)
  (define/public (on-receive msg)
    this))
                                         
                           
;; Match -> Scene
(define (draw-match m)
  (local [(define (draw-player c trail scn)
            (foldl (λ (p scn)
                     (place-image 
                      (square GRID-SIZE "solid" c)
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
                           
                            
                     




