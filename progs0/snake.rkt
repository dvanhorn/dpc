#lang class1

;; The Snake Game in Object-Oriented Style
;; Copyright (c) 2010 David Van Horn
;; Licensed under the Academic Free License version 3.0

(require class1/universe)
(require 2htdp/image)

(define SNAKE-COLOR "red")
(define FOOD-COLOR  "green")

(define CELL-SIZE 24) ; Size of a board square in pixels;
; must be an even number to keep pixel coords integers.
(define HALF-CELL (/ CELL-SIZE 2)) ; handy
(define BOARD-HEIGHT/CELLS 10)
(define BOARD-HEIGHT/PIXELS (* BOARD-HEIGHT/CELLS CELL-SIZE))

(define BOARD-WIDTH/CELLS 10)
(define BOARD-WIDTH/PIXELS (* BOARD-WIDTH/CELLS CELL-SIZE))

(define SEG-IMG     (circle (/ CELL-SIZE 2) "solid" SNAKE-COLOR))
(define FOOD-IMG    (circle (/ CELL-SIZE 2) "solid" FOOD-COLOR))
(define EMPTY-BOARD (empty-scene BOARD-WIDTH/PIXELS BOARD-HEIGHT/PIXELS))
(define LOSER-IMG (text "You lose" 44 'purple))

;; Seg  = (new seg% [x Nat] [y Nat])
;; Food = (new food% [x Nat] [y Nat])
;; Snake = (new snake% [dir Dir] [segs [Listof Seg]])
;; World = (new world% [snake Snake] [food Food])
;; Dir = (U "up" "down" "left" "right")

;;; The board is divided into cells; each cell is CELL-SIZE x CELL-SIZE
;;; pixels on the rendered scene.
;;; The "pixel coordinate system" has (0,0) at the top/left corner.
;;; The "cell coordinate system" has (0,0) at the lower/left corner.

(define-class cell% 
  (fields x y)
  
  ;; Posn -> Boolean
  (define/public (=? p)
    (and (= (field x) (send p x))
         (= (field y) (send p y))))
  
  ;; Nat Nat -> Cell
  (define/public (mod mx my)
    (new cell%
         (modulo (field x) mx)
         (modulo (field y) my)))
  
  ;; -> Nat
  ;; Produce the x-coord of the pixel at the *center* of this cell.
  (define/public (x->pixel)      
    (+ HALF-CELL (* (send this x) CELL-SIZE)))
  
  ;; -> Nat
  ;; Produce the y-coord of the pixel at the *center* of this cell.
  (define/public (y->pixel)
    (* CELL-SIZE (- BOARD-HEIGHT/CELLS (+ 1/2 (send this y)))))
  
  ;; Image Scene -> Scene
  (define/public (+img+scene img scn)
    (place-image img
                 (x->pixel)
                 (y->pixel)
                 scn)))

(check-expect (send (new cell% 1 2) =? 
                    (new cell% 1 2))
              true)
(check-expect (send (new cell% 6 4) mod 5 3)
              (new cell% 1 1))

(check-expect (send (new cell% 0 0) x->pixel) HALF-CELL)
(check-expect (send (new cell% 4 0) x->pixel) (* CELL-SIZE 9/2))
(check-expect (send (new cell% 0 0) y->pixel)
              (- BOARD-HEIGHT/PIXELS HALF-CELL))
(check-expect (send (new cell% 0 4) y->pixel) 
              (- BOARD-HEIGHT/PIXELS (* 9/2 CELL-SIZE)))

(check-expect (send (new cell% 0 0) +img+scene SEG-IMG EMPTY-BOARD)
              (place-image SEG-IMG 
                           HALF-CELL 
                           (- BOARD-HEIGHT/PIXELS HALF-CELL)
                           EMPTY-BOARD))

(define-class seg%
  (super cell%)
  
  ;; Nat Nat -> Seg
  (define/public (move dx dy)
    (new seg% 
         (+ (send this x) dx) 
         (+ (send this y) dy)))
  
  ;; Dir -> Seg
  (define/public (move-dir d)
    (cond [(string=? d "up")    (send this move 0 1)]
          [(string=? d "down")  (send this move 0 -1)]
          [(string=? d "left")  (send this move -1 0)]
          [(string=? d "right") (send this move 1 0)]))
  
  ;; Scene -> Scene
  (define/public (+scene scn)
    (send this +img+scene SEG-IMG scn)))

(check-expect (send (new seg% 1 2) move 2 4)
              (new seg% 3 6))
(check-expect (send (new seg% 0 0) move 1 1)
              (new seg% 1 1))

(check-expect (send (new seg% 0 0) move-dir "up")
              (new seg% 0 1))
(check-expect (send (new seg% 0 0) move-dir "down")
              (new seg% 0 -1))
(check-expect (send (new seg% 0 0) move-dir "left")
              (new seg% -1 0))
(check-expect (send (new seg% 0 0) move-dir "right")
              (new seg% 1 0))



(define-class food%
  (super cell%)
  
  ;; Scene -> Scene
  (define/public (+scene scn)
    (send this +img+scene FOOD-IMG scn)))

(define (drop-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs)
                    (drop-last (rest segs)))]))

(define-class snake%
  (fields dir segs)
  
  ;;; (cons Seg [Listof Seg]) -> [Listof Seg]
  ;;; Drop the last segment from the list of segs.
  
  (define/public (drop-last segs)
    (cond [(empty? (rest segs)) empty]
          [else (cons (first segs)
                      (drop-last (rest segs)))]))
  
  (define/public (head) ;; -> Seg
    (first (field segs)))
    
  ;; Dir -> Snake
  ;; Change direction of this snake.
  (define/public (change-dir dir)
    (new snake% dir (field segs)))
    
  ;; -> Seg
  ;; Compute the next head segment.
  (define/public (next-head) 
    (send (head) move-dir (field dir)))
    
  ;; Food -> Boolean
  ;; Is this snake eating the given food?
  (define/public (eating? food)
    (send food =? (head)))
    
  ;; -> Boolean
  ;; Is this snake colliding with itself?
  (define/public (self-collide?)
    (ormap (λ (s) (send s =? (head)))
           (rest (field segs))))
  
  ;; -> Boolean
  ;; Is this snake colliding with any of the walls?
  (define/public (wall-collide?)
    (not (and (< -1 (send (head) x) BOARD-WIDTH/CELLS)
              (< -1 (send (head) y) BOARD-HEIGHT/CELLS))))
  
  ;; Slither this snake forward one segment.
  ;; -> Snake
  (define/public (slither)
    (new snake%
         (field dir)
         (cons (next-head)
               (drop-last (field segs)))))
  
  ;; Grow this snake one segment.
  ;; -> Snake
  (define/public (grow)
    (new snake%
         (field dir)
         (cons (next-head) (field segs))))
  
  ;; Scene -> Scene
  (define/public (+scene scn)
    (foldr (λ (s scn) (send s +scene scn)) scn (field segs))))


(define snake0
  (new snake%
       "right"
       (list (new seg% 4 5)
             (new seg% 3 5)
             (new seg% 2 5))))


(check-expect (send snake0 slither)
              (new snake%
                   "right"
                   (list (new seg% 5 5)
                         (new seg% 4 5)
                         (new seg% 3 5))))


(define origin-cell (new cell% 0 0))

(check-expect (send origin-cell +img+scene SEG-IMG EMPTY-BOARD)
              (place-image SEG-IMG 
                           (send origin-cell x->pixel)
                           (send origin-cell y->pixel)
                           EMPTY-BOARD))
(check-expect (send (new seg%  0 0) +scene EMPTY-BOARD)
              (send (new cell% 0 0) +img+scene SEG-IMG EMPTY-BOARD))

(define-class world%
  (fields snake food)
  
  ;; -> World
  (define/public (eat&grow)
    (new world% ;; Potential this%. 
         (send (field snake) grow)
         (new food% 
              (random BOARD-WIDTH/CELLS)
              (random BOARD-HEIGHT/CELLS))))
  
  ;; -> World
  (define/public (on-tick)
    (cond [(send (field snake) eating? (field food)) (eat&grow)]
          [else (new world% ;; Potential this%.
                     (send (field snake) slither)
                     (field food))]))
    
    ;; -> Boolean
    (define/public (stop-when)
      (or (send (field snake) wall-collide?)
          (send (field snake) self-collide?)))
    
    ;; -> Scene
    (define/public (to-draw)
      (send (field snake) +scene 
            (send (field food) +scene EMPTY-BOARD)))
    
    ;; -> Scene
    (define/public (last-image)
      (place-image LOSER-IMG
                   (quotient BOARD-HEIGHT/PIXELS 2)
                   (quotient BOARD-WIDTH/PIXELS 2)
                   (to-draw)))    
  
  (define/public (tick-rate) 1/5)
    
    ;; Key -> World
    (define/public (on-key k)
      (new world% ;; Potential this% 
           (send (field snake) change-dir
                        (cond [(key=? k "up")    k]
                              [(key=? k "down")  k]
                              [(key=? k "left")  k]
                              [(key=? k "right") k]
                              [else 
                               (send (field snake) dir)]))
           (field food))))



#|
;; Plays a game with segments that wrap.
(define modulo-seg%
  (class* seg% ()
    (super-new)
    (inspect false)
    (define/override (move-dir d)
      (send (super move-dir d) mod
            BOARD-WIDTH/CELLS
            BOARD-HEIGHT/CELLS))))
|#
(define food0
  (new food% 7 7))

(define world0
  (new world% 
       snake0
       food0))

(define world-eating
  (new world%
       (new snake% 
            "right"
            (list (new seg% 
                       (send food0 x)
                       (send food0 y))))
       food0))

(check-expect (send world0 on-tick)
              (new world%
                   (send snake0 slither)
                   (send world0 food)))

(check-expect (send (send world-eating on-tick) snake)
              (send (send world-eating snake) grow))    

(check-expect (send (send world-eating snake) eating?
                    (send world-eating food))
              true)

(check-expect (send world0 to-draw)
              (send snake0 +scene
                    (send food0 +scene EMPTY-BOARD)))
(check-expect (send world0 last-image)
              (place-image LOSER-IMG
                           (quotient BOARD-HEIGHT/PIXELS 2)
                           (quotient BOARD-WIDTH/PIXELS 2)
                           (send world0 to-draw)))

(check-expect (send (send world0 eat&grow) snake)
              (send snake0 grow))

(check-expect (send world0 stop-when) false)

(check-expect (send world0 on-key "a") world0)             
(check-expect (send world0 on-key "up")
              (new world%
                   (send (send world0 snake) change-dir "up")
                   (send world0 food)))
(check-expect (send world0 on-key "down")
              (new world%
                   (send (send world0 snake) change-dir "down")
                   (send world0 food)))
(check-expect (send world0 on-key "left")
              (new world%
                    (send (send world0 snake) change-dir "left")
                    (send world0 food)))
(check-expect (send world0 on-key "right")
              (new world%
                   (send (send world0 snake) change-dir "right")
                   (send world0 food)))


(big-bang world0)
#;
(test)

; Play with bounding walls.
;(play seg%)
; Play in modulo mode.
;(play modulo-seg%)

#;
(equal? (new snake% [dir "right"] [segs null]) (new snake% [dir "right"] [segs null]))
