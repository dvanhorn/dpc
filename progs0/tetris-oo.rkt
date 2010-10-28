#lang racket
;; Tetris in Object-Oriented Style
;; Copyright (c) 2010 David Van Horn
;; Licensed under the Academic Free License version 3.0

(require racket/class)
(require 2htdp/universe)
(require 2htdp/image)
(require test-engine/racket-tests)

(define block-size   20)  ;; in Pixels
(define board-width  10)  ;; in Blocks
(define board-height 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Definitions

;; A Block is a (new block% [x Nat] [y Nat] [color Color])
(define block% 
  (class* object% ()
    (super-new)
    (init-field [(the-x x)]
                [(the-y y)]
                [(the-color color)])
    (define/public (x) the-x)
    (define/public (y) the-y)
    (define/public (color) the-color)
    
    ;; Nat Nat -> Block
    (define/public (move dx dy)
      (new this%
           [x (+ the-x dx)]
           [y (+ the-y dy)]
           [color the-color]))
    
    ;; Block -> Boolean
    (define/public (=? b)
      (and (= the-x (send b x))
           (= the-y (send b y))))
    
    ;; Posn -> Block
    (define/public (rotate-ccw p)
      (new this% 
           [x (+ (send p x)
                 (- (send p y)
                    the-y))]
           [y (+ (send p y)
                 (- the-x
                    (send p x)))]
           [color the-color]))
    
    ;; Posn -> Block
    (define/public (rotate-cw p)
      (send (send (rotate-ccw p) rotate-ccw p) rotate-ccw p))
    
    ;; -> Image
    (define/public (->image)
      (rectangle (add1 block-size) (add1 block-size) 'solid the-color))
    
    ;; Scene -> Scene
    (define/public (+scene scn)
      (place-image (->image)
                   (+ (* the-x block-size) (/ block-size 2))
                   (+ (* the-y block-size) (/ block-size 2))
                   scn))))


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

(define set-interface
  (interface () 
    empty?    ;; -> Boolean
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
;; - (new mt-set% [= [X X -> Boolean]])
;; - (new cons-set% [elem X] [rest [Set X]]),
;;   where elem is *not* an element in rest.
      
(define set% 
  (class* object% ()
    (super-new)
    (init-field [(the-= =)])
 
    ;; -> [X X -> Boolean]
    (define/public (=) the-=)
    
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
            (new mt-set% [= the-=])))
    
    ;; [X -> Y] [Y Y -> Boolean] -> [Set Y]
    (define/public (map f =)
      (send this fold 
            (λ (x s) (send s add (f x)))
            (new mt-set% [= =])))))

(define mt-set%
  (class* set% (set-interface)
    (super-new)
    (define/public (empty?) true)
    (define/public (contains? x) false)
    (define/public (remove x)
      this)
    (define/public (add x)
      (new cons-set% [elem x] [rest this]))
    (define/public (intersect s)
      this)
    (define/public (subset? s)
      true)
    (define/public (fold f b)
      b)))
      

(define cons-set%
  (class* set% (set-interface)
    (init-field [(the-elem elem)] [(the-rest rest)])
    (super-new [= (send the-rest =)])
    (inherit =)
    
    ;; -> Boolean
    (define/public (empty?) false)
    
    ;; X -> Boolean
    (define/public (contains? x)
      (or ((=) the-elem x)
          (send the-rest contains? x)))
    
    ;; X -> [Set X]
    (define/public (remove x)
      (if ((=) x the-elem)
          the-rest
          (new this% 
               [elem the-elem]
               [rest (send the-rest remove x)])))
    
    ;; [Set X] -> [Set X]
    (define/public (intersect s)
      (if (send s contains? the-elem)
          (new this%
               [elem the-elem]
               [rest (send the-rest intersect s)])
          (send the-rest intersect s)))
    
    ;; [Set X] -> Boolean
    (define/public (subset? s)
      (and (send s contains? the-elem)
           (send the-rest subset? s)))
    
    ;; X -> [Set X]
    (define/public (add x)
      (cond [(contains? x) this]
            [else
             (new this% [elem x] [rest this])]))
    
    ;; [X Y -> Y] Y -> Y
    (define/public (fold f b)
      (f the-elem (send the-rest fold f b)))))
   
;; [X X -> Bolean] X ... -> [Set X]
;; Convenient constructor for sets.
(define (set = . xs)
  (cond [(empty? xs) (new mt-set% [= =])]
        [else (send (apply set = (rest xs)) add (first xs))]))
                


(check-expect (send (set =) empty?) true)
(check-expect (send (set =) contains? 5) false)
(check-expect (send (send (set =) remove 5) empty?) true)
(check-expect (send (send (set =) union (set =)) empty?) true)
(check-expect (send (send (set =) intersect (set =)) empty?) true)

(check-expect (send (set = 1) =? (set = 1)) true)
(check-expect (send (set = 1) empty?) false)
(check-expect (send (set = 1) contains? 5) false)
(check-expect (send (set = 1 5 2) contains? 5) true)
(check-expect (send (send (set = 1) remove 5) empty?) false)
(check-expect (send (send (set = 1) union (set =)) empty?) false)
(check-expect (send (send (set = 1) intersect (set =)) empty?) true)

(check-expect (send (set = 1 1 1) =? (set = 1)) true)
(check-expect (send (set = 1) =? (set = 1 1 1)) true)
(check-expect (send (set = 1 2 3) =? (set = 3 2 1)) true)

(check-expect (send (set = 1 2 3 4) fold + 0) 10)


;; Nat Nat -> Boolean
(define (=%5 x y) (= (modulo x 5) (modulo y 5)))
(check-expect (send (set =%5 1 2 3) =? (set =%5 6 7 8)) true)

;; [Set X] [Set X] -> Boolean
(define (s= s1 s2)
  (send s1 =? s2))

;; Comparing [Set [Set Nat]]
(check-expect (send (set s= (set = 1 2 3) (set = 3 2 1)) =?
                    (set s= (set = 1 2 3)))
              true)
              
(check-expect (send (send (set = 1 2 3 6 7 8) map add1 =) contains? 1) false)
(check-expect (send (send (set = 1 2 3 6 7 8) map add1 =) contains? 2) true)
(check-expect (send (send (set = 1 2 3 6 7 8) map add1 =%5) length) 3)
              

;; -----------------------------------------------------------------------
;; POSN

(define posn%
  (class* object% ()
    (super-new)
    (init-field [(the-x x)] [(the-y y)])
    (define/public (x) the-x) ;; -> Nat
    (define/public (y) the-y) ;; -> Nat
    ;; Posn -> Boolean
    (define/public (=? p)
      (and (= the-x (send p x))
           (= the-y (send p y))))
    ;; Nat Nat -> Posn
    (define/public (move dx dy)
      (new this% [x (+ the-x dx)] [y (+ the-y dy)]))))


;; -----------------------------------------------------------------------
(define EMPTY-BOARD
  (empty-scene (add1 (* board-width block-size)) 
               (add1 (* board-height block-size))))
                         
;; A Tetra is a (new tetra% [center Posn] [blocks [Set Block]])
;; The center point is the point around which the tetra rotates
;; when it is rotated.
(define tetra%
  (class* object% ()
    (super-new)
    (init-field [(the-center center)]
                [(the-blocks blocks)])
    (define/public (center) the-center)
    (define/public (blocks) the-blocks)
    (define/public (move dx dy)
      (new this% 
           [center (send the-center move dx dy)]
           [blocks (send the-blocks map 
                         (λ (b) (send b move dx dy))
                         (λ (b1 b2) (send b1 =? b2)))]))
    
    (define/public (rotate-ccw)
      (new tetra% 
           [center the-center]
           [blocks (send the-blocks map
                         (λ (b) (send b rotate-ccw the-center))
                         (λ (b1 b2) (send b1 =? b2)))]))
    
    (define/public (rotate-cw)
      (new tetra% 
           [center the-center]
           [blocks (send the-blocks map
                         (λ (b) (send b rotate-cw the-center))
                         (λ (b1 b2) (send b1 =? b2)))]))
    
    (define/public (+scene scene)
      (send the-blocks fold 
            (λ (b scn) (send b +scene scn))
            EMPTY-BOARD))
            
    
    (define/public (overlaps? bs)
      (not (send (send bs intersect the-blocks) empty?)))
))

(define (block=? b1 b2) (send b1 =? b2))

(define (build-tetra-blocks color xc yc x1 y1 x2 y2 x3 y3 x4 y4)
  (send (new tetra% 
             [center (new posn% [x xc] [y yc])]
             [blocks (set block=?
                          (new block% [x x1] [y y1] [color color])
                          (new block% [x x2] [y y2] [color color])
                          (new block% [x x3] [y y3] [color color])
                          (new block% [x x4] [y y4] [color color]))])
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
   (build-tetra-blocks 'pink    1   -1      0 -2 1 -2 1 -1 2 -1)
   ))

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
(define world%
  (class* object% ()
    (super-new)
    (init-field [(the-tetra tetra)]
                [(the-blocks blocks)])
    (define/public (tetra) the-tetra)
    (define/public (blocks) the-blocks)
    ;; -> World
    (define/public (touchdown)
      (new this% 
           [tetra (list-pick-random tetras)]
           [blocks (send (send (send the-tetra blocks) union the-blocks)
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
           [tetra (send the-tetra rotate-ccw)]
           [blocks the-blocks]))
    (define/public (rotate-cw)
      (new world% 
           [tetra (send the-tetra rotate-cw)]
           [blocks the-blocks]))
    (define/public (move dx dy)
      (try-new-tetra (send the-tetra move dx dy)))
    (define/public (try-new-tetra t)
      (cond [(or (<  (blocks-min-x (send t blocks)) 0)
                 (>= (blocks-max-x (send t blocks)) board-width)
                 (send t overlaps? the-blocks))
             this]
	[else (new this% [tetra% t] [blocks the-blocks])]))
    
    ))
    
(define (blocks-min-x bs)
  (send bs fold (λ (b n) (min (send b x) n)) board-width))
(define (blocks-max-x bs)
  (send bs fold (λ (b n) (max (send b x) n)) 0))

                  

(test)

