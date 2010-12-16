;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snake) (read-case-sensitive #f) (teachpacks ()) (htdp-settings #(#f write mixed-fraction #f #f none #f ())))
;; The Snake Game in Object-Oriented Style
;; Copyright (c) 2010 David Van Horn
;; Licensed under the Academic Free License version 3.0

(require classN) ;; racket/class
(require 2htdp/universe)
(require 2htdp/image)



(define CELL-SIZE 24) ; Size of a board square in pixels;
                      ; must be an even number to keep pixel coords integers.
(define HALF-CELL (/ CELL-SIZE 2)) ; handy
(define BOARD-HEIGHT/CELLS 10)
(define BOARD-HEIGHT/PIXELS (* BOARD-HEIGHT/CELLS CELL-SIZE))

(define BOARD-WIDTH/CELLS 10)
(define BOARD-WIDTH/PIXELS (* BOARD-WIDTH/CELLS CELL-SIZE))




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
  (local [(define (decode % n hi-y)
            (new % 
                 [x (quotient n hi-y)]
                 [y (remainder n hi-y)]))
          
          (define (encode p hi-y)
            (+ (* (send p x) hi-y)
               (send p y)))]
    
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
             [y (modulo the-y my)]))
      
      (define/public (random-not-in lop hi-x hi-y)
        ;; [Listof Posn] Nat Nat -> This%      
        (decode this%
                (random-nat-not-in (map (λ (p) (encode p hi-y)) lop)
                                   (* hi-x hi-y))
                hi-y)))))


                  
                  
       
#|
(check-expect (encode (new posn% [x 0] [y 0]) 5) 0)
(check-expect (encode (new posn% [x 1] [y 0]) 5) 5)
(check-expect (encode (new posn% [x 0] [y 1]) 5) 1)

(check-expect (decode posn% 0 5) (new posn% [x 0] [y 0]))
(check-expect (decode posn% 5 5) (new posn% [x 1] [y 0]))
(check-expect (decode posn% 1 5) (new posn% [x 0] [y 1]))

(check-expect (decode food% 0 5) (new food% [x 0] [y 0]))
(check-expect (decode food% 5 5) (new food% [x 1] [y 0]))
(check-expect (decode food% 1 5) (new food% [x 0] [y 1]))

(check-expect (send (new posn% [x 5] [y 5])
                    random-not-in empty 1 1)
              (new posn% [x 0] [y 0]))

(check-expect (random-posn-not-in posn% (list (new posn% [x 0] [y 1])) 1 2)
              (new posn% [x 0] [y 0]))
    
(check-expect (random-posn-not-in food% empty 1 1)
              (new food% [x 0] [y 0]))
(check-expect (random-posn-not-in food% (list (new posn% [x 0] [y 1])) 1 2)
              (new food% [x 0] [y 0]))
   
|#

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

(define SOME-IMG (circle 10 "solid" "red"))
#;
(check-expect (send (new cell% [x 0] [y 0]) +img+scene SOME-IMG EMPTY-BOARD)
              (place-image SOME-IMG 
                           HALF-CELL 
                           (- BOARD-HEIGHT/PIXELS HALF-CELL)
                           EMPTY-BOARD))



(define seg%
  (class* cell% ()
    (super-new)
    (inspect false)
    
    (define/public (image)
      (overlay (circle (/ CELL-SIZE 2) "outline" 
                       "black")
               (circle (/ CELL-SIZE 2) "solid" 
                       (send this color))))
    
    (define/public (color)
      "red")
      
    ;; Dir -> Seg
    (define/public (move-dir d)
      (cond [(string=? d "up")    (send this move 0 1)]
            [(string=? d "down")  (send this move 0 -1)]
            [(string=? d "left")  (send this move -1 0)]
            [(string=? d "right") (send this move 1 0)]))  
    
    ;; Scene -> Scene
    (define/public (+scene scn)
      (send this +img+scene (send this image) scn))))

(define food%
  (class* cell% ()
    (super-new)
    (inspect false)
    
    (define/public (image)
      (circle (/ CELL-SIZE 2) "solid"
              (send this color)))
    
    (define/public (color)
      "green")

    ;; Grow a new food.
    ;; -> This%
    (define/public (grow)
       (new this%
            [x (random BOARD-WIDTH/CELLS)]
            [y (random BOARD-HEIGHT/CELLS)]))
      
    ;; Scene -> Scene
    (define/public (+scene scn)
      (send this +img+scene (send this image) scn))))

(define snake%
  (local [;;; This is copasetic with equality.
          ;;; drop-last : NESegs -> Segs
          ;;; Drop the last segment from the list of segs.
          (define (drop-last nesegs)
            (cond [(empty? (rest nesegs)) empty]
                  [else (cons (first nesegs)
                              (drop-last (rest nesegs)))]))]
    (class* object% ()
      (super-new)
      (init-field [(the-dir dir)] [(the-segs segs)])        
      (inspect false)
      
      ;; This causes snake%s to be incomparable with equal?.
      #;
      (define/value (drop-last nesegs)
        (cond [(empty? (rest nesegs)) empty]
              [else (cons (first nesegs)
                          (drop-last (rest nesegs)))]))
                        
      (define/public (dir)  ;; -> Dir
        the-dir)  
      (define/public (segs) ;; -> [Listof Seg]
        the-segs) 
      (define/public (head) ;; -> Seg
        (first the-segs))
      
      ;; Dir -> Snake
      ;; Change direction of this snake.
      (define/public (change-dir dir)
        (new this% [dir dir] [segs the-segs]))
      
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
      (define/public (wall-collide? p)
        (not (and (< -1 (send (head) x) (send p x))
                  (< -1 (send (head) y) (send p x)))))
      
      ;; Slither this snake forward one segment.
      ;; -> This%
      (define/public (slither)
        (new this%
             [dir the-dir]
             [segs (cons (next-head)
                         (drop-last the-segs))]))
      
      ;; Grow this snake one segment.
      ;; -> This%
      (define/public (grow)
        (new this%
             [dir the-dir]
             [segs (cons (next-head) the-segs)]))
      
      ;; Scene -> Scene
      (define/public (+scene scn)
        (foldr (λ (s scn) (send s +scene scn)) scn the-segs)))))
    
(define snake0
  (new snake%
       [dir "right"]
       [segs (list (new seg% [x 4] [y 5]))]))

(check-expect snake0 
              (new snake%
                   [dir "right"]
                   [segs (list (new seg% [x 4] [y 5]))]))

(define origin-cell (new cell% [x 0] [y 0]))
#;
(check-expect (send origin-cell +img+scene SOME-IMG EMPTY-BOARD)
              (place-image SOME-IMG 
                           (send origin-cell x->pixel)
                           (send origin-cell y->pixel)
                           EMPTY-BOARD))

(check-expect snake0 snake0)

(define world%
  (class* object% ()
    (super-new)
    (init-field [(the-snake snake)] [(the-food food)])
    (inspect false)
    
    (define/public (snake) the-snake)
    (define/public (food) the-food)
    
    (define/public (rate) 1/5)
    
    (define/value d (new cell% [x 10] [y 10]))
    (define/public (DIMENSIONS) d)
    
    (define/value i 
       (empty-scene BOARD-WIDTH/PIXELS 
                    BOARD-HEIGHT/PIXELS))
    #;
    (empty-scene (send (DIMENSIONS) x->pixel)
                   (send (DIMENSIONS) y->pixel))
    
    (define/public (EMPTY-BOARD) i)
   
    
    ;; -> World
    (define/public (eat&grow)
      (new this%
           [snake (send the-snake grow)]
           [food (send the-food grow)]))
    
    ;; -> World
    (define/public (step)
      (cond [(send the-snake eating? the-food) (eat&grow)]
            [else (new this%
                       [snake (send the-snake slither)]
                       [food the-food])]))
    
    ;; -> Boolean
    (define/public (game-over?)
      (or (send the-snake wall-collide? (send this DIMENSIONS))
          (send the-snake self-collide?)))
    
    ;; -> Scene
    (define/public (->scene)
      (send the-snake +scene 
            (send the-food +scene
                  (send this EMPTY-BOARD))))
    
    ;; -> Scene
    (define/public (->loser-scene)
      (overlay (text "You lose" 44 'purple)
               (send the-snake +scene 
                     (send the-food +scene 
                           (send this EMPTY-BOARD)))))
    
    ;; Key -> World
    (define/public (handle-key k)
      (new this%
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

;; ----------------------------------------------------------
;; Mixins

;; World% -> World%
;; A class of worlds never places food on the snake.
(define (fancy-<%> %)
  (class* % ()
    (super-new)
    (inspect false)
    (define/override (eat&grow)
      (new this%
           [snake (send (send this snake) grow)]
           [food 
            (send (send this food)
                  random-not-in 
                  (send (send this snake) segs)
                  BOARD-WIDTH/CELLS
                  BOARD-HEIGHT/CELLS)]))))

(define (screwed-<%> r)
  (λ (%)
    (class* % ()
      (super-new)
      (inspect false)
      (define/override (rate)
        (* (super rate) r)))))

;; Seg% -> Seg%
(define (star-seg-<%> %)
  (class* % ()
    (super-new)
    (inspect false)
    (define/override (image)
      (overlay (star CELL-SIZE "outline" "black")
               (star CELL-SIZE "solid" (send this color))))))
    
    
;; Seg% -> Seg%
;; A class of segments that wrap.
(define (modulo-<%> %)
  (class* % ()
    (super-new)
    (inspect false)
    (define/override (move-dir d)
      (send (super move-dir d) mod
            BOARD-WIDTH/CELLS
            BOARD-HEIGHT/CELLS))))

;; IColor% -> IColor%
;; A class of segments that are drawn with random color.
(define (random-colors-<%> %)
  (class* % ()
    (super-new)
    (inspect false)
    (define/override (color)
      (make-color (random 256) 
                  (random 256)
                  (random 256)))))

;; IColor% -> IColor%
;; A class of % with random color.
(define (random-color-<%> %)
  (local [(define c (make-color (random 256) 
                                (random 256)
                                (random 256)))]    
    (class* % ()
      (super-new)
      (inspect false)
      (define/override (color) c))))

(define (add-random-color-<%> %)
  (local [(define c (make-color (random 256) 
                                (random 256)
                                (random 256)))]
    (class* % ()
      (super-new)
      (inspect false)
      (define/public (color) c))))


(define (set-color-<%> c)
  (λ (%)
    (class* % ()
      (super-new)
      (inspect false)
      (define/override (color) c))))

;; Color -> % -> IColor%
(define (add-color-<%> c)
  (λ (%)
    (class* % ()
      (super-new)
      (inspect false)
      (define/public (color) c))))

(define (color-background-<%> %)
  (class* % ()
    (super-new)
    (inspect false)
    (define/override (EMPTY-BOARD)
      (let ((mt (super EMPTY-BOARD)))
        (rectangle (image-width mt)
                   (image-height mt)
                   "solid"
                   (send this color))))))

(define (no-opposite-change-dir-<%> %)
  (local [(define (opposite? d1 d2)
            (or (and (string=? d1 "up")
                     (string=? d2 "down"))
                (and (string=? d1 "down")
                     (string=? d2 "up"))
                (and (string=? d1 "left")
                     (string=? d2 "right"))
                (and (string=? d1 "right")
                     (string=? d2 "left"))))]
    
    (class* % ()
      (super-new)
      (inspect false)
      ;; Dir -> Snake
      ;; Change direction of this snake unless its opposite.
      (define/override (change-dir dir)
        (cond [(opposite? dir (send this dir)) this]
              [else 
                (new this% 
                     [dir dir] 
                     [segs (send this segs)])])))))

       
                 
;; The mutation doesn't interact well with big-bang.x
(define (rotate-image-<%> %)
  (class* % ()
    (super-new)
    (inspect false)
    (field [angle 0])
            
    (define/override (image)
      (set-field! angle this (modulo (+ 5 angle) 360))
      (rotate angle (super image)))))

(define (zoom-world-<%> factor)
  (λ (%)
    (class* % ()
      (super-new)
      (inspect false)
      (define/override (->scene)
        (scale factor (super ->scene)))
      
      (define/override (->loser-scene)
        (scale factor (super ->loser-scene))))))  


(define (zoom-image-<%> r)
  (λ (%)
    (class* % ()
      (super-new)
      (inspect false)
      (define/override (image)
        (scale r (super image))))))

(define (restart-when-game-over-<%> restart) 
  (λ (%)
  (class* % ()
    (super-new)
    (inspect false)
    (define/override (step)
      (let ((next (super step)))
        (if (send next game-over?)
            (restart this)
            next))))))

(define (n-to-advance-<%> n)
  (λ (%)
    (class* % ()
      (super-new)
      (inspect false)
      (define/public (next-level?)
        (< n (length (send (send this snake) segs)))))))
      
(define (set-n-to-advance-<%> n)
  (λ (%)
    (class* % ()
      (super-new)
      (inspect false)
      (define/override (next-level?)
        (< n (length (send (send this snake) segs)))))))

(define (next-level-<%> level)
  (λ (%)
    (class* % ()
      (super-new)
      (inspect false)
      (define/override (step)
        (let ((next (super step)))
          (if (send next next-level?)
              (level next)
              next))))))

(define (truncate-snake-<%> %)
  (class* % ()
    (super-new)
    (inspect false)
    (define/public (truncate)
      (new this% 
           [dir (send this dir)]
           [segs (list (first (send this segs)))]))))

(define (no-suicide-snake-<%> %)
  (class* % ()
    (super-new)
    (inspect false)
    (define/override (change-dir dir)
      (local [(define cd (super change-dir dir))]
        (cond [(send (send cd slither) self-collide?)
               this]
              [else cd])))))

;; Kids always want to place to food on some random place
;; that's not on the snake.  Let's do it.

;; [Listof Nat] Nat -> Nat
;; Where:
;; (< (length lon) hi)
;; (< -1 (list-ref lon i) hi)
;; Elements are distinct
(define (random-nat-not-in lon hi)
  (foldl (λ (n i) 
           (if (<= n i)
               (add1 i)
               i))
         (random (- hi (length lon)))
         (sort lon <)))
 
(check-expect (random-nat-not-in empty 1) 0)
(check-expect (random-nat-not-in (list 0 1) 3) 2)
(check-expect (random-nat-not-in (list 1 2) 3) 0)






(define (play w)
  (big-bang w
            (on-tick (λ (w) (send w step)) 
                     (send w rate))
            (to-draw (λ (w) (send w ->scene)))
            (on-key  (λ (w k) (send w handle-key k)))
            (stop-when (λ (w) (send w game-over?))
                       (λ (w) (send w ->loser-scene)))))


(define (make-w0 world snake food seg)
  (new world
       [snake (new snake
                   [dir "right"]
                   [segs (list (new seg [x 4] [y 5])
                               (new seg [x 3] [y 5])
                               (new seg [x 2] [y 5]))])]
       [food (new food [x 7] [y 7])]))


(define w0 (make-w0
            ((restart-when-game-over-<%> (λ (_) w0))
             ((screwed-<%> 1/2)
              (fancy-<%>
               ((zoom-world-<%> 2)
                (color-background-<%>
                 (random-color-<%>
                  ((add-color-<%> "forestgreen")
                   world%)))))))
            (no-opposite-change-dir-<%> 
             snake%)
            ((set-color-<%> "gold")
             ((zoom-image-<%> 2)
              (star-seg-<%> 
               food%)))
            (modulo-<%> 
             ((zoom-image-<%> 1.5) seg%))))

(define w0% 
  ((next-level-<%> (λ (wn)
                     (new ((set-n-to-advance-<%> 
                            (* 2 (sub1 (length (send (send wn snake) segs)))))
                           (random-color-<%> w0%))
                          [food (send wn food)]
                          [snake (send (send wn snake) truncate)])))
   ((n-to-advance-<%> 4)
    (color-background-<%> 
     ((add-color-<%> "forestgreen")
       world%)))))

(define w1
  (make-w0 
   w0%
   (truncate-snake-<%> snake%)
   food%
   (modulo-<%> seg%)))
  

(play w0)
#;(play w1)

;; Basic game ®
#;
(play (make-w0 (fancy-<%> 
                (color-background-<%>
                 ((add-color-<%> "pink")
                  ((zoom-world-<%> 2)
                   world%))))
               (no-suicide-snake-<%> snake%)
               ((set-color-<%> "yellow") food%)
               (modulo-<%> 
                ((zoom-image-<%> 1.2) seg%))))




