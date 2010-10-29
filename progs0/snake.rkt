;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; The Snake Game in Object-Oriented Style
;; Copyright (c) 2010 David Van Horn
;; Licensed under the Academic Free License version 3.0

;; lang ISL+
(require class0) ;; define-class
(require 2htdp/universe)
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
         [x (modulo (field x) mx)]
         [y (modulo (field y) my)]))
  
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

(check-expect (send (new cell% [x 1] [y 2]) =? 
                    (new cell% [x 1] [y 2]))
              true)
(check-expect (send (new cell% [x 6] [y 4]) mod 5 3)
              (new cell% [x 1] [y 1]))

(check-expect (send (new cell% [x 0] [y 0]) x->pixel) HALF-CELL)
(check-expect (send (new cell% [x 4] [y 0]) x->pixel) (* CELL-SIZE 9/2))
(check-expect (send (new cell% [x 0] [y 0]) y->pixel)
              (- BOARD-HEIGHT/PIXELS HALF-CELL))
(check-expect (send (new cell% [x 0] [y 4]) y->pixel) 
              (- BOARD-HEIGHT/PIXELS (* 9/2 CELL-SIZE)))

(check-expect (send (new cell% [x 0] [y 0]) +img+scene SEG-IMG EMPTY-BOARD)
              (place-image SEG-IMG 
                           HALF-CELL 
                           (- BOARD-HEIGHT/PIXELS HALF-CELL)
                           EMPTY-BOARD))

(define-class seg%
  (super cell%)
  
  ;; Nat Nat -> Seg
  (define/public (move dx dy)
    (new seg% 
         [x (+ (send this x) dx)] 
         [y (+ (send this y) dy)]))
  
  ;; Dir -> Seg
  (define/public (move-dir d)
    (cond [(string=? d "up")    (send this move 0 1)]
          [(string=? d "down")  (send this move 0 -1)]
          [(string=? d "left")  (send this move -1 0)]
          [(string=? d "right") (send this move 1 0)]))
  
  ;; Scene -> Scene
  (define/public (+scene scn)
    (send this +img+scene SEG-IMG scn)))

(check-expect (send (new seg% [x 1] [y 2]) move 2 4)
              (new seg% [x 3] [y 6]))
(check-expect (send (new seg% [x 0] [y 0]) move 1 1)
              (new seg% [x 1] [y 1]))

(check-expect (send (new seg% [x 0] [y 0]) move-dir "up")
              (new seg% [x 0] [y 1]))
(check-expect (send (new seg% [x 0] [y 0]) move-dir "down")
              (new seg% [x 0] [y -1]))
(check-expect (send (new seg% [x 0] [y 0]) move-dir "left")
              (new seg% [x -1] [y 0]))
(check-expect (send (new seg% [x 0] [y 0]) move-dir "right")
              (new seg% [x 1] [y 0]))



(define-class food%
  (super cell%)
  
  ;; Scene -> Scene
  (define/public (+scene scn)
    (send this +img+scene FOOD-IMG scn)))

;;; (cons Seg [Listof Seg]) -> [Listof Seg]
;;; Drop the last segment from the list of segs.
(define (drop-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs)
                    (drop-last (rest segs)))]))

(define-class snake%
  (fields dir segs)
  
  (define/public (head) ;; -> Seg
    (first (field segs)))
    
  ;; Dir -> Snake
  ;; Change direction of this snake.
  (define/public (change-dir dir)
    (new snake% [dir dir] [segs (field segs)]))
    
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
         [dir (field dir)]
         [segs (cons (next-head)
                     (drop-last (field segs)))]))
  
  ;; Grow this snake one segment.
  ;; -> Snake
  (define/public (grow)
    (new snake%
         [dir (field dir)]
         [segs (cons (next-head) (field segs))]))
  
  ;; Scene -> Scene
  (define/public (+scene scn)
    (foldr (λ (s scn) (send s +scene scn)) scn (field segs))))


(define snake0
  (new snake%
       [dir "right"]
       [segs (list (new seg% [x 4] [y 5])
                   (new seg% [x 3] [y 5])
                   (new seg% [x 2] [y 5]))]))

(check-expect (send snake0 slither)
              (new snake%
                   [dir "right"]
                   [segs (list (new seg% [x 5] [y 5])
                               (new seg% [x 4] [y 5])
                               (new seg% [x 3] [y 5]))]))


(define origin-cell (new cell% [x 0] [y 0]))

(check-expect (send origin-cell +img+scene SEG-IMG EMPTY-BOARD)
              (place-image SEG-IMG 
                           (send origin-cell x->pixel)
                           (send origin-cell y->pixel)
                           EMPTY-BOARD))
(check-expect (send (new seg%  [x 0] [y 0]) +scene EMPTY-BOARD)
              (send (new cell% [x 0] [y 0]) +img+scene SEG-IMG EMPTY-BOARD))

(define-class world%
  (fields snake food)
  ;; -> World
  (define/public (eat&grow)
    (new world% ;; Potential this%. 
         [snake (send (field snake) grow)]
         [food (new food% 
                    [x (random BOARD-WIDTH/CELLS)]
                    [y (random BOARD-HEIGHT/CELLS)])]))
  
  ;; -> World
  (define/public (step)
    (cond [(send (field snake) eating? (field food)) (eat&grow)]
          [else (new world% ;; Potential this%.
                     [snake (send (field snake) slither)]
                     [food (field food)])]))
    
    ;; -> Boolean
    (define/public (game-over?)
      (or (send (field snake) wall-collide?)
          (send (field snake) self-collide?)))
    
    ;; -> Scene
    (define/public (->scene)
      (send (field snake) +scene 
            (send (field food) +scene EMPTY-BOARD)))
    
    ;; -> Scene
    (define/public (->loser-scene)
      (place-image LOSER-IMG
                   (quotient BOARD-HEIGHT/PIXELS 2)
                   (quotient BOARD-WIDTH/PIXELS 2)
                   (->scene)))
    
    ;; Key -> World
    (define/public (handle-key k)
      (new world% ;; Potential this% 
           [snake (send (field snake) change-dir
                        (cond [(key=? k "up")    k]
                              [(key=? k "down")  k]
                              [(key=? k "left")  k]
                              [(key=? k "right") k]
                              [else 
                               (send (field snake) dir)]))]
           [food (field food)])))



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
  (new food% [x 7] [y 7]))

(define world0
  (new world% 
       [snake snake0]
       [food food0]))

(define world-eating
  (new world%
       [snake (new snake% 
                   [dir "right"]
                   [segs (list (new seg% 
                                    [x (send food0 x)]
                                    [y (send food0 y)]))])]
       [food food0]))

(check-expect (send world0 step)
              (new world%
                   [snake (send snake0 slither)]
                   [food (send world0 food)]))

(check-expect (send (send world-eating step) snake)
              (send (send world-eating snake) grow))    

(check-expect (send (send world-eating snake) eating?
                    (send world-eating food))
              true)

(check-expect (send world0 ->scene)
              (send snake0 +scene
                    (send food0 +scene EMPTY-BOARD)))
(check-expect (send world0 ->loser-scene)
              (place-image LOSER-IMG
                           (quotient BOARD-HEIGHT/PIXELS 2)
                           (quotient BOARD-WIDTH/PIXELS 2)
                           (send world0 ->scene)))

(check-expect (send (send world0 eat&grow) snake)
              (send snake0 grow))

(check-expect (send world0 game-over?) false)

(check-expect (send world0 handle-key "a") world0)             
(check-expect (send world0 handle-key "up")
              (new world%
                   [snake (send (send world0 snake) change-dir "up")]
                   [food (send world0 food)]))
(check-expect (send world0 handle-key "down")
              (new world%
                   [snake (send (send world0 snake) change-dir "down")]
                   [food (send world0 food)]))
(check-expect (send world0 handle-key "left")
              (new world%
                   [snake (send (send world0 snake) change-dir "left")]
                   [food (send world0 food)]))
(check-expect (send world0 handle-key "right")
              (new world%
                   [snake (send (send world0 snake) change-dir "right")]
                   [food (send world0 food)]))


(big-bang world0
 (on-tick (λ (w) (send w step)) 1/5)
 (to-draw (λ (w) (send w ->scene)))
 (on-key  (λ (w k) (send w handle-key k)))
 (stop-when (λ (w) (send w game-over?))
            (λ (w) (send w ->loser-scene))))

; Play with bounding walls.
;(play seg%)
; Play in modulo mode.
;(play modulo-seg%)


