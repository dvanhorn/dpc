#lang class0
;; Tetris in Object-Oriented Style
;; Copyright (c) 2010 David Van Horn
;; Licensed under the Academic Free License version 3.0

;; FIXME: not complete, not fully tested, not fully documented.

(require 2htdp/universe)
(require 2htdp/image)

(define block-size   20)  ;; in Pixels
(define board-width  10)  ;; in Blocks
(define board-height 20)


;; A Block is a (new block% [x Nat] [y Nat] [color Color])
(define-class block% 
  (fields x y color)
    
  ;; Nat Nat -> Block
  (define/public (move dx dy)
    (new block%
         [x (+ (field x) dx)]
         [y (+ (field y) dy)]
         [color (field color)]))
  
  ;; Block -> Boolean
  (define/public (=? b)
    (and (= (field x) (send b x))
         (= (field y) (send b y))))
  
  ;; Posn -> Block
  (define/public (rotate-ccw p)
    (new block% 
         [x (+ (send p x)
               (- (send p y)
                  (field y)))]
         [y (+ (send p y)
               (- (field x)
                  (send p x)))]
         [color (field color)]))
    
    ;; Posn -> Block
    (define/public (rotate-cw p)
      (send (send (rotate-ccw p) rotate-ccw p) rotate-ccw p))
    
    ;; -> Image
    (define/public (->image)
      (rectangle (add1 block-size) (add1 block-size) 'solid (field color)))
    
    ;; Scene -> Scene
    (define/public (+scene scn)
      (place-image (->image)
                   (+ (* (field x) block-size) (/ block-size 2))
                   (+ (* (field y) block-size) (/ block-size 2))
                   scn)))


(check-expect (send (new block% [x 0] [y 0] [color 'black]) =?
                    (new block% [x 0] [y 0] [color 'black]))
              true)
(check-expect (send (new block% [x 0] [y 0] [color 'black]) =?
                    (new block% [x 0] [y 0] [color 'red]))
              true)
(check-expect (send (new block% [x 0] [y 0] [color 'black]) =?
                    (new block% [x 0] [y 1] [color 'black]))
              false)

(check-expect (send (send (new block% [x 0] [y 0] [color 'black]) move 0 0) =?
                    (new block% [x 0] [y 0] [color 'black]))
              true)
(check-expect (send (send (new block% [x 0] [y 0] [color 'black]) move 0 1) =?
                    (new block% [x 0] [y 1] [color 'black]))
              true)
              

;; A Blocks is a (new blocks% [elems [Listof Block]])
;; where elems contains no duplicates.
(define-class blocks% 
  (fields elems)
 
  ;; [Set X] -> Boolean
  (define/public (=? s)
    (and (subset? s)
         (send s subset? this)))
  
  ;; [Set X] -> [Set X]
  (define/public (union s)
    (fold (λ (y s) (send s add y)) s))
  
  ;; -> Nat
  (define/public (length)
    (fold (λ (_ n) (add1 n)) 0))
  
  ;; [X -> Boolean] -> [Set X]
  (define/public (filter f)
    (fold (λ (x s) (if (f x)
                       (send s add x)
                       s))
          (new blocks% [elems empty])))
  
  ;; [X -> Y] -> [Set Y]
  (define/public (map f)
    (fold (λ (x s) (send s add (f x)))
          (new blocks% [elems empty])))
  
  (define/public (empty?)
    (not (cons? (field elems))))
  
  (define/public (contains? x)
    (ormap (λ (y) (send x =? y))
           (field elems)))
  
  (define/public (remove x)
    (new blocks%
         [elems
          (foldl (λ (y xs) 
                   (cond [(send x =? y)
                          xs]
                         [else
                          (cons y xs)]))
                 empty
                 (field elems))]))
  
  (define/public (add x)
    (cond [(ormap (λ (y) (send x =? y))
                  (field elems))
           this]
          [else
           (new blocks% 
                [elems (cons x (field elems))])]))
  
  (define/public (intersect s)
    (new blocks% 
         [elems
          (foldl (λ (x xs) 
                   (cond [(ormap (λ (y) (send x =? y))
                                 (send s elems))
                          (cons x xs)]
                         [else xs]))
                 empty
                 (field elems))]))
  
  (define/public (subset? s)
    (andmap (λ (x) (send s contains? x))
            (field elems)))
  
  (define/public (fold f b)
    (foldl f b (field elems)))
  
  
  ;; Nat -> Boolean
  (define/public (full-row? i)
    (= board-width
       (send (row i) length)))
  
  ;; -> Blocks
  (define/public (row i)
    (filter (λ (b) (= (send b y) i))))
  
  ;; -> Blocks
  (define/public (eliminate-full-rows)
    (local [(define (elim-row i offset)
              (cond [(< i 0) mt-blocks]
                    [(full-row? i)   (elim-row (sub1 i) (add1 offset))]
                    [else (send (elim-row (sub1 i) offset) union
                                (send (row i) move 0 offset))]))]
      (elim-row board-height 0)))
  
  ;; -> Nat
  (define/public (min-x)
    (fold (λ (b n) (min (send b x) n)) board-width))
  (define/public (max-x)
    (fold (λ (b n) (max (send b x) n)) 0))
  (define/public (max-y)
    (fold (λ (b n) (max (send b y) n)) 0))
  
  ;; -> Boolean
  (define/public (overflow?)
    (ormap (λ (b) (<= (send b y) 0))
           (field elems)))

  ;; Nat Nat -> Blocks
  ;; Move each block by the given X & Y displacement.
  (define/public (move dx dy)
    (map (λ (b) (send b move dx dy))))


  
  ;; -> Scene
  (define/public (->scene)
    (fold (λ (b img)
            (cond [(<= 0 (send b y)) (send b +scene img)]
                  [else img]))
          (empty-scene (add1 (* board-width block-size)) 
                       (add1 (* board-height block-size))))))

  
  
  

(define-class posn%
  (fields x y)
  ;; Posn -> Boolean
  (define/public (=? p)
    (and (= (field x) (send p x))
         (= (field y) (send p y))))
  ;; Nat Nat -> Posn
  (define/public (move dx dy)
    (new posn% [x (+ (field x) dx)] [y (+ (field y) dy)])))


;; -----------------------------------------------------------------------
(define EMPTY-BOARD
  (empty-scene (add1 (* board-width block-size)) 
               (add1 (* board-height block-size))))
          
;; A Tetra is a (new tetra% [center Posn] [blocks Blocks])
;; The center point is the point around which the tetra rotates
;; when it is rotated.
(define-class tetra%
  (fields center blocks)
  
  (define/public (move dx dy)
    (new tetra% 
         [center (send (field center) move dx dy)]
         [blocks (send (field blocks) map 
                       (λ (b) (send b move dx dy)))]))                       
  
  (define/public (rotate-ccw)
    (new tetra% 
         [center (field center)]
         [blocks (send (field blocks) map
                       (λ (b) (send b rotate-ccw (field center))))]))
  
  (define/public (rotate-cw)
    (new tetra% 
         [center (field center)]
         [blocks (send (field blocks) map
                       (λ (b) (send b rotate-cw (field center))))]))
  (define/public (+scene scene)
    (send (field blocks) fold 
          (λ (b scn) (send b +scene scn))
          EMPTY-BOARD))
    
  (define/public (overlaps? bs)
    (not (send (send bs intersect (field blocks)) empty?))))

(define (build-tetra-blocks color xc yc x1 y1 x2 y2 x3 y3 x4 y4)
  (send (new tetra% 
             [center (new posn% [x xc] [y yc])]
             [blocks (new blocks%
                          [elems (list (new block% [x x1] [y y1] [color color])
                                       (new block% [x x2] [y y2] [color color])
                                       (new block% [x x3] [y y3] [color color])
                                       (new block% [x x4] [y y4] [color color]))])])
        move 3 0))

(define mt-blocks 
  (new blocks% [elems empty]))
        

;; Bogus centers
(define tetras
  (list 
   (build-tetra-blocks 'green  	1/2 -3/2    0 -1 0 -2 1 -1 1 -2)
   (build-tetra-blocks 'blue   	1   -1      0 -1 1 -1 2 -1 3 -1)
   (build-tetra-blocks 'purple 	1   -1      0 -1 1 -1 2 -1 2 -2)
   (build-tetra-blocks 'cyan   	1   -1      0 -1 1 -1 2 -1 0 -2)
   (build-tetra-blocks 'orange  1   -1      0 -1 1 -1 2 -1 1 -2)
   (build-tetra-blocks 'red     1   -1      0 -1 1 -1 1 -2 2 -2)
   (build-tetra-blocks 'pink    1   -1      0 -2 1 -2 1 -1 2 -1)))

;; list-pick-random : [Listof a] -> a
;; Randomly pick an element from the list.
(define (list-pick-random ls)
  (list-ref ls (random (length ls))))

;; A World is a (new world% [tetra Tetra] [blocks [Set Block]])
(define-class world%
  (fields tetra blocks)
  
  ;; -> World
  (define/public (touchdown)
    (new world% 
         [tetra (list-pick-random tetras)]
         [blocks (send (send (send (field tetra) blocks) union 
                             (field blocks))
                       eliminate-full-rows)]))
  
  ;; -> Boolean
  ;; Has the current tetra in this world landed on blocks?
  ;; I.e., if we move the tetra down 1, will it touch any existing blocks?
  (define/public (landed-on-blocks?)
    (send (send (field tetra) move 0 1) overlaps? 
          (field blocks)))
  
  ;; -> Boolean
  ;; Has the current tetra in this world landed on the floor?
  (define/public (landed-on-floor?)
    (= (send (send (field tetra) blocks) max-y)
       (sub1 board-height)))
  
  ;; -> Boolean
  ;; Has the current tetra landed?
  (define/public (landed?)
    (or (landed-on-blocks?)
        (landed-on-floor?)))
  
  ;; -> World
  ;; Step this world, either touchdown or move the tetra down on step.
  (define/public (next)
    (cond [(landed?) (touchdown)]
          [else (new world%
                     [tetra (send (field tetra) move 0 1)]
                     [blocks (field blocks)])]))
  
  ;; Tetra -> World
  ;; Make a world with the new tetra *IF* if doesn't lie on top of some other
  ;; block or lie off the board. Otherwise, no change.
  (define/public (try-new-tetra new-tetra)
    (cond [(or (<  (send (send new-tetra blocks) min-x) 0)
               (>= (send (send new-tetra blocks) max-x) board-width)
               (send new-tetra overlaps? (field blocks)))
           this]
	[else 
         (new world% 
              [tetra new-tetra]
              [blocks (field blocks)])]))
  
  ;; Number Number -> World
  ;; Move this world's tetra by the given X & Y displacement, but only if you can.
  ;; Otherwise stay put.
  (define/public (move dx dy)
    (try-new-tetra (send (field tetra) move dx dy)))
  
  ;; -> World
  ;; Rotate this world's tetra 90 degrees counterclockwise, but only if you can.
  ;; Otherwise stay put.
  (define/public (rotate-ccw)
    (new world% 
         [tetra (send (field tetra) rotate-ccw)]
         [blocks (field blocks)]))
  
  ;; -> World
  ;; Rotate this world's tetra 90 degrees clockwise, but only if you can.
  ;; Otherwise stay put.  
  (define/public (rotate-cw)
    (new world% 
         [tetra (send (field tetra) rotate-cw)]
         [blocks (field blocks)]))
  
  ;; Key -> World
  ;; Move this world according to the given key event.
  (define/public (handle-key k)
    (cond [(key=? k "left")
           (move -1 0)]
          [(key=? k "right")
           (move 1 0)]
          [(key=? k "a")
           (rotate-ccw)]
          [(key=? k "s")
           (rotate-cw)]
          [else this]))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Visualization
  
  ;; -> Scene
  ;; Visualize whirled peas
  (define/public (->image)
    (place-image (send (send (send (field tetra) blocks)
                             union
                             (field blocks))
                       ->scene)
                 0 0 
                 (empty-scene (* board-width block-size)
                              (* board-height block-size))))
  


  
  )

(define world0
  (new world% 
       [tetra (list-pick-random tetras)]
       [blocks mt-blocks]))


(big-bang world0 
          (on-tick (λ (w) (send w next)) (/ 1.0 5))
          (stop-when (λ (w) (send (send w blocks) overflow?)))
          (on-draw (λ (w) (send w ->image)))
          (on-key (λ (w k) (send w handle-key k))))


