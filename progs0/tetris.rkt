#lang class0
;; Tetris in Object-Oriented Style
;; Copyright (c) 2010 David Van Horn
;; Licensed under the Academic Free License version 3.0

(require 2htdp/universe)
(require 2htdp/image)

(define block-size   20)  ;; in Pixels
(define board-width  10)  ;; in Blocks
(define board-height 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

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
              

;; -----------------------------------------------------------------------
;; Set library

;; This library uses equivalence relations, represented as 
;; functions [X X -> Boolean] to define set operations.

;; It is useful for representing sets of values.

(define-interface set-interface
  (empty?    ;; -> Boolean
   contains? ;; X -> Boolean
   remove    ;; X -> [Set X]
   union     ;; [Set X] -> [Set X]
   intersect ;; [Set X] -> [Set X]
   =?        ;; [Set X] -> Boolean
   subset?   ;; [Set X] -> Bolean
   map       ;; [X -> Y] [Y Y -> Boolean] -> [Set Y]
   length    ;; -> Nat
   fold      ;; [X Y -> Y] Y -> Y
   filter))  ;; [X -> Boolean] -> [Set X]

;; A [Set X] is one of:
;; - (new set% [= [X X -> Boolean]] [elems [Listof X]])
;; where elems contains no duplicates.
(define-class set% 
  (fields = elems)
 
  ;; [Set X] -> Boolean
  (define/public (=? s)
    (and (send this subset? s)
         (send s subset? this)))
  
  ;; [Set X] -> [Set X]
  (define/public (union s)
    (send this fold (λ (y s) (send s add y)) s))
  
  ;; -> Nat
  (define/public (length)
    (send this fold (λ (_ n) (add1 n)) 0))
  
  ;; [X -> Boolean] -> [Set X]
  (define/public (filter f)
    (send this fold
          (λ (x s) (if (f x)
                       (send s add x)
                       s))
          (new set% 
               [= (field =)]
               [elems empty])))
  
  ;; [X -> Y] [Y Y -> Boolean] -> [Set Y]
  (define/public (map f =)
    (send this fold 
          (λ (x s) (send s add (f x)))
          (new set% 
               [= =]
               [elems empty])))
  
  (define/public (empty?)
    (not (cons? (field elems))))
  
  (define/public (contains? x)
    (ormap (λ (y) ((field =) x y))
           (field elems)))
  
  (define/public (remove x)
    (new set%
         [= (field =)]
         [elems
          (foldl (λ (y xs) 
                   (cond [((field =) x y)
                          xs]
                         [else
                          (cons y xs)]))
                 empty
                 (field elems))]))
  
  (define/public (add x)
    (cond [(ormap (λ (y) ((field =) x y))
                  (field elems))
           this]
          [else
           (new set% 
                [= (field =)]
                [elems (cons x (field elems))])]))
  
  (define/public (intersect s)
    (new set% 
         [= (field =)]
         [elems
          (foldl (λ (x xs) 
                   (cond [(ormap (λ (y) ((field =) x y))
                                 (send s elems))
                          (cons x xs)]
                         [else xs]))
                 empty
                 (field elems))]))
  
  (define/public (subset? s)
    (andmap (λ (x) (send s contains? x))
            (field elems)))
  
  (define/public (fold f b)
    (foldl f b (field elems))))
  
  
;; [X X -> Bolean] [Listof X] -> [Set X]
;; Convenient constructor for sets.
(define (set = xs)
  (new set% [= =] [elems xs]))

(check-expect (send (set = empty) empty?) true)
(check-expect (send (set = empty) contains? 5) false)
(check-expect (send (send (set = empty) remove 5) empty?) true)
(check-expect (send (send (set = empty) union (set = empty)) empty?) true)
(check-expect (send (send (set = empty) intersect (set = empty)) empty?) true)

(check-expect (send (set = (list 1)) =? (set = (list 1))) true)
(check-expect (send (set = (list 1)) empty?) false)
(check-expect (send (set = (list 1)) contains? 5) false)
(check-expect (send (set = (list 1 5 2)) contains? 5) true)
(check-expect (send (send (set = (list 1)) remove 5) empty?) false)
(check-expect (send (send (set = (list 1)) union (set = empty)) empty?) false)
(check-expect (send (send (set = (list 1)) intersect (set = empty)) empty?) true)

(check-expect (send (set = (list 1 1 1)) =? (set = (list 1))) true)
(check-expect (send (set = (list 1)) =? (set = (list 1 1 1))) true)
(check-expect (send (set = (list 1 2 3)) =? (set = (list 3 2 1))) true)

(check-expect (send (set = (list 1 2 3 4)) fold + 0) 10)


;; Nat Nat -> Boolean
(define (=%5 x y) (= (modulo x 5) (modulo y 5)))
(check-expect (send (set =%5 (list 1 2 3)) =? (set =%5 (list 6 7 8))) true)

;; [Set X] [Set X] -> Boolean
(define (s= s1 s2)
  (send s1 =? s2))

;; Comparing [Set [Set Nat]]
(check-expect (send (set s= (list (set = (list 1 2 3)) (set = (list 3 2 1)))) =?
                    (set s= (list (set = (list 1 2 3)))))
              true)
              
(check-expect (send (send (set = (list 1 2 3 6 7 8)) map add1 =) contains? 1) false)
(check-expect (send (send (set = (list 1 2 3 6 7 8)) map add1 =) contains? 2) true)
(check-expect (send (send (set = (list 1 2 3 6 7 8)) map add1 =%5) length) 3)
              

;; -----------------------------------------------------------------------
;; POSN

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
          
;; A Tetra is a (new tetra% [center Posn] [blocks [Set Block]])
;; The center point is the point around which the tetra rotates
;; when it is rotated.
(define-class tetra%
  (fields center blocks)
  
  (define/public (move dx dy)
    (new tetra% 
         [center (send (field center) move dx dy)]
         [blocks (send (field blocks) map 
                       (λ (b) (send b move dx dy))
                       (λ (b1 b2) (send b1 =? b2)))]))
  
  (define/public (rotate-ccw)
    (new tetra% 
         [center (field center)]
         [blocks (send (field blocks) map
                       (λ (b) (send b rotate-ccw (field center)))
                       (λ (b1 b2) (send b1 =? b2)))]))
  
  (define/public (rotate-cw)
    (new tetra% 
         [center (field center)]
         [blocks (send (field blocks) map
                       (λ (b) (send b rotate-cw (field center)))
                       (λ (b1 b2) (send b1 =? b2)))]))
  
  (define/public (+scene scene)
    (send (field blocks) fold 
          (λ (b scn) (send b +scene scn))
          EMPTY-BOARD))
    
  (define/public (overlaps? bs)
    (not (send (send bs intersect (field blocks)) empty?))))

(define (block=? b1 b2) (send b1 =? b2))

(define (build-tetra-blocks color xc yc x1 y1 x2 y2 x3 y3 x4 y4)
  (send (new tetra% 
             [center (new posn% [x xc] [y yc])]
             [blocks (set block=?
                          (list (new block% [x x1] [y y1] [color color])
                                (new block% [x x2] [y y2] [color color])
                                (new block% [x x3] [y y3] [color color])
                                (new block% [x x4] [y y4] [color color])))])
        move 3 0))
        

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

(define (full-row? bs i)
  (= board-width
     (send (blocks-row bs) length)))

(define (blocks-row bs i)
  (send bs filter (λ (b) (= (send b y) i))))

(define (eliminate-full-rows bs)
  (local [(define (elim-row i offset)
            (cond [(< i 0) (set block=?)]
                  [(full-row? bs i)   (elim-row (sub1 i) (add1 offset))]
                  [else (send (elim-row (sub1 i) offset) union
                              (send (blocks-row bs i) move 0 offset))]))]
    (elim-row board-height 0)))


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
  ;; Key -> World
  (define/public (handle-key k)
    (cond [(key=? k "left")
           (move -1 0)]
          [(key=? k "right")
           (move 1 0)]
          [(key=? k "down")
           (jumpdown)]
          [(key=? k "a")
           (rotate-ccw)]
          [(key=? k "s")
           (rotate-cw)]
          [else this]))
    
    ;; -> World
    (define/public (jumpdown) '...)
    (define/public (rotate-ccw)
      (new world% 
           [tetra (send (field tetra) rotate-ccw)]
           [blocks (field blocks)]))
    (define/public (rotate-cw)
      (new world% 
           [tetra (send (field tetra) rotate-cw)]
           [blocks (field blocks)]))
    (define/public (move dx dy)
      (try-new-tetra (send (field tetra) move dx dy)))
    (define/public (try-new-tetra t)
      (cond [(or (<  (blocks-min-x (send t blocks)) 0)
                 (>= (blocks-max-x (send t blocks)) board-width)
                 (send t overlaps? (field blocks)))
             this]
	[else (new world% [tetra% t] [blocks (field blocks)])])))
    
(define (blocks-min-x bs)
  (send bs fold (λ (b n) (min (send b x) n)) board-width))
(define (blocks-max-x bs)
  (send bs fold (λ (b n) (max (send b x) n)) 0))

                  
