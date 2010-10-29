;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snake) (read-case-sensitive #f) (teachpacks ()) (htdp-settings #(#f write mixed-fraction #f #f none #f ())))
;; The Snake Game in Object-Oriented Style
;; Copyright (c) 2010 David Van Horn
;; Licensed under the Academic Free License version 3.0

(require classN) ;; racket/class
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

;;; drop-last : NESegs -> Segs
;;; Drop the last segment from the list of segs.
(define (drop-last nesegs)
  (cond [(empty? (rest nesegs)) empty]
        [else (cons (first nesegs)
                    (drop-last (rest nesegs)))]))

(check-expect (drop-last (list 1 2 3)) (list 1 2))

;; Seg  = (new seg% [x Nat] [y Nat])
;; Food = (new food% [x Nat] [y Nat])
;; Snake = (new snake% [dir Dir] [segs [Listof Seg]])
;; World = (new world% [snake Snake] [food Food])
;; Dir = (U "up" "down" "left" "right")

;;; The board is divided into cells; each cell is CELL-SIZE x CELL-SIZE
;;; pixels on the rendered scene.
;;; The "pixel coordinate system" has (0,0) at the top/left corner.
;;; The "cell coordinate system" has (0,0) at the lower/left corner.

(define posn% 
  (class* object% ()
    (super-new)
    (inspect false)
    (init-field [(the-x x)] [(the-y y)])
    (define/public (x) the-x)
    (define/public (y) the-y)

    ;; Posn -> Boolean
    (define/public (=? p)
      (and (= the-x (send p x))
           (= the-y (send p y))))
    
    ;; Nat Nat -> This%
    (define/public (move dx dy)
      (new this% 
           [x (+ the-x dx)] 
           [y (+ the-y dy)]))
    
    ;; Nat Nat -> This%
    (define/public (mod mx my)
      (new this% 
           [x (modulo the-x mx)]
           [y (modulo the-y my)]))))


(check-expect (send (new posn% [x 1] [y 2]) move 2 4)
              (new posn% [x 3] [y 6]))
(check-expect (send (new posn% [x 1] [y 2]) =? 
                    (new posn% [x 1] [y 2]))
              true)

(define cell%
  (class* posn% ()
    (super-new)
    (inspect false)
    
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
                   scn))))

;; Thanks, this%!
(check-expect (send (new cell% [x 0] [y 0]) move 1 1)
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



(define seg%
  (class* cell% ()
    (super-new)
    (inspect false)
    
    ;; Dir -> Seg
    (define/public (move-dir d)
      (cond [(string=? d "up")    (send this move 0 1)]
            [(string=? d "down")  (send this move 0 -1)]
            [(string=? d "left")  (send this move -1 0)]
            [(string=? d "right") (send this move 1 0)]))  
    
    ;; Scene -> Scene
    (define/public (+scene scn)
      (send this +img+scene SEG-IMG scn))))

(define food%
  (class* cell% ()
    (super-new)
    (inspect false)
    
    ;; Scene -> Scene
    (define/public (+scene scn)
      (send this +img+scene FOOD-IMG scn))))

(define snake%
  (class* object% ()
    (super-new)
    (init-field [(the-dir dir)] [(the-segs segs)])
        
    (define/public (dir)  ;; -> Dir
      the-dir)  
    (define/public (segs) ;; -> [Listof Seg]
      the-segs) 
    (define/public (head) ;; -> Seg
      (first the-segs))
    
    ;; Dir -> Snake
    ;; Change direction of this snake.
    (define/public (change-dir dir)
      (new snake% [dir dir] [segs the-segs]))
    
    ;; -> Seg
    ;; Compute the next head segment.
    (define/public (next-head) 
      (send (head) move-dir the-dir))
    
    ;; Food -> Boolean
    ;; Is this snake eating the given food?
    (define/public (eating? food)
      (send food =? (head)))
    
    ;; -> Boolean
    ;; Is this snake colliding with itself?
    (define/public (self-collide?)
      (ormap (λ (s) (send s =? (head)))
             (rest the-segs)))
    
    ;; -> Boolean
    ;; Is this snake colliding with any of the walls?
    (define/public (wall-collide?)
      (not (and (< -1 (send (head) x) BOARD-WIDTH/CELLS)
                (< -1 (send (head) y) BOARD-HEIGHT/CELLS))))
    
    ;; Slither this snake forward one segment.
    ;; -> Snake
    (define/public (slither)
      (new snake%
           [dir the-dir]
           [segs (cons (next-head)
                       (drop-last the-segs))]))
    
    ;; Grow this snake one segment.
    ;; -> Snake
    (define/public (grow)
      (new snake%
           [dir the-dir]
           [segs (cons (next-head) the-segs)]))
    
    ;; Scene -> Scene
    (define/public (+scene scn)
      (foldr (λ (s scn) (send s +scene scn)) scn the-segs))))
    

(define origin-cell (new cell% [x 0] [y 0]))

(check-expect (send origin-cell +img+scene SEG-IMG EMPTY-BOARD)
              (place-image SEG-IMG 
                           (send origin-cell x->pixel)
                           (send origin-cell y->pixel)
                           EMPTY-BOARD))
(check-expect (send (new seg%  [x 0] [y 0]) +scene EMPTY-BOARD)
              (send (new cell% [x 0] [y 0]) +img+scene SEG-IMG EMPTY-BOARD))

(define world%
  (class* object% ()
    (super-new)
    (init-field [(the-snake snake)] [(the-food food)])
    ;; -> World
    (define/public (eat&grow)
      (new world% ;; Potential this%. 
           [snake (send the-snake grow)]
           [food (new food% 
                      [x (random BOARD-WIDTH/CELLS)]
                      [y (random BOARD-HEIGHT/CELLS)])]))
    
    ;; -> World
    (define/public (step)
      (cond [(send the-snake eating? the-food) (eat&grow)]
            [else (new world% ;; Potential this%.
                       [snake (send the-snake slither)]
                       [food the-food])]))
    
    ;; -> Boolean
    (define/public (game-over?)
      (or (send the-snake wall-collide?)
          (send the-snake self-collide?)))
    
    ;; -> Scene
    (define/public (->scene)
      (send the-snake +scene 
            (send the-food +scene EMPTY-BOARD)))
    
    ;; -> Scene
    (define/public (->loser-scene)
      (place-image (text "You lose" 44 'purple)
                   (quotient BOARD-HEIGHT/PIXELS 2)
                   (quotient BOARD-WIDTH/PIXELS 2)
                   (->scene)))
    
    ;; Key -> World
    (define/public (handle-key k)
      (new world% ;; Potential this% 
           [snake (send the-snake change-dir
                        (cond [(key=? k "up")    k]
                              [(key=? k "down")  k]
                              [(key=? k "left")  k]
                              [(key=? k "right") k]
                              [else 
                               (send the-snake dir)]))]
           [food the-food]))))


(define origin (new posn% [x 0] [y 0]))
(check-expect (send (new posn% [x 1] [y 2]) x) 1)
(check-expect (send origin =? origin) true)
(check-expect (send (send origin move 10 20) =?
                    (new posn% [x 10] [y 20]))
              true)

;; Plays a game with segments that wrap.
(define modulo-seg%
  (class* seg% ()
    (super-new)
    (inspect false)
    (define/override (move-dir d)
      (send (super move-dir d) mod
            BOARD-WIDTH/CELLS
            BOARD-HEIGHT/CELLS))))

(define (play <seg%>)
  (big-bang
   (new world% 
        [snake (new snake%
                    [dir "right"]
                    [segs (list (new <seg%> [x 4] [y 5])
                                (new <seg%> [x 3] [y 5])
                                (new <seg%> [x 2] [y 5]))])]
        [food (new food% [x 7] [y 7])])
   (on-tick (λ (w) (send w step)) 1/5)
   (to-draw (λ (w) (send w ->scene)))
   (on-key  (λ (w k) (send w handle-key k)))
   (stop-when (λ (w) (send w game-over?))
              (λ (w) (send w ->loser-scene)))))

; Play with bounding walls.
(play seg%)
; Play in modulo mode.
;(play modulo-seg%)


