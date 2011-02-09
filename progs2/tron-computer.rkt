#lang class2
;; RUBRIC
;; ========================================================

;; Problem set 4, Problem 2 (total 50 points)

;; This problem is supposed to be written in class1, but 
;; the solution is in class2 just for the convenience of
;; leaving of `new'.

;; This rubric is fairly open-ended because the design is
;; fairly open-ended.

;; World
;; -----

;; 10 Points for clear, sensible data definitions for the
;; state of the world.
;; Should include some way of modeling:
;; - initial world (waiting for server to start play)
;; - world in play

;; For all world classes (in total):
;; 10 Points for purpose statements
;; 10 Points for contracts
;; 10 Points method definitions (code)
;; 10 Points for tests

;; Students need not do anything interesting with their
;; computer player.

;; =========================================================
;; Computer Tron

(require class1/universe)
(require 2htdp/image)
(require "tron.rkt")

;; =========================================================
;; Computer

;; A Computer is one of:
;; - (cmt%)
;; - (cmatch% (list Trail Trail))
;; - (final% Outcome (list Trail Trail))

;; An Outcome is one of: 'win, 'lose, 'draw

;; This computer player drives around in a square.

;; -> Universe World Computer
;; Play a computer player locally.
(define (play-local-computer)
  (launch-many-worlds (serve)
                      (play LOCALHOST)
                      (big-bang (cmt% LOCALHOST))))

(define STEPS-UNTIL-TURN 10)

(define-class cmt%
  (fields ip)
  
  ;; -> String
  ;; IP address where world should register.
  (check-expect ((cmt% LOCALHOST) . register) LOCALHOST)
  (define/public (register) (field ip))
  
  ;; -> Scene
  ;; Render this waiting world as a scene.
  (check-expect ((cmt% LOCALHOST) . to-draw) WAITING)
  (define/public (to-draw) WAITING)
  
  ;; (list Trail Trail) -> Package
  ;; Start playing with given trails.
  (check-expect ((cmt% LOCALHOST) . on-receive '(() ()))
                (make-package 
                 (cmatch% '(() ()) STEPS-UNTIL-TURN "left")
                 "down"))
  (define/public (on-receive msg)   
    (make-package (cmatch% msg 
                           STEPS-UNTIL-TURN 
                           (next-dir "down"))
                  "down")))

(define-class cmatch%
  (fields m steps dir)
  
  ;; -> Scene
  ;; Render this match as a scene.
  (define/public (to-draw)
    (place-image (text "Computer" 30 "black")
                 100 50
                 (draw-match (field m))))
  
  ;; (U Outcome (list Trail Trail)) -> (U World Package)
  ;; Handle an outcome or new state.
  (check-expect ((cmatch% '(() ()) 10 "down") . on-receive 'draw)
                (final% 'draw '(() ())))
  (check-expect ((cmatch% '(() ()) 10 "down") . on-receive '((()) ()))
                (cmatch% '((()) ()) 9 "down"))
  (define/public (on-receive msg)
    (cond [(outcome? msg) (final% msg (field m))]
          [(zero? (field steps))
           (make-package (cmatch% msg 
                                  STEPS-UNTIL-TURN 
                                  (next-dir (field dir)))
                         (field dir))]
          [else (cmatch% msg 
                         (sub1 (field steps))
                         (field dir))])))

;; Dir -> Dir
;; The next direction in a square.
(define (next-dir d)
  (cond [(string=? "down" d) "left"]
        [(string=? "left" d) "up"]
        [(string=? "up" d) "right"]
        [(string=? "right" d) "down"]))