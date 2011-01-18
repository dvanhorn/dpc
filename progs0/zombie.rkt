#lang class0
;; ==========================================================
;; Play the classic game of Zombie Brains!

;; All zombies move towards the player.  The player moves 
;; toward the mouse. Zombies collision cause flesh heaps 
;; that are deadly to other zombies and the player.  
;; Randomly teleport via mouse click as a last resort!

;; Based on Robot!, p. 234 of Barski's Land of Lisp.
(require 2htdp/image)
(require class0/universe)

;; A World is a (world Posn [Listof Posn] [Listof Posn]).
;; A Posn is a Int + (* +i Int).

(define-class world%
  (fields player zombies junk mouse-posn)
  
  (define/public (name)
    "Zombie Attack!")
  
  (define/public (on-tick)
    (send (junk-it) move))
  
  (define/public (tick-rate)
    1/10)    
  
  ;; -> Scene
  (define/public (to-draw)
    (foldr (posn+scn "gray")
           (foldr (posn+scn "red") 
                  ((posn+scn "green")
                   (field player)
                   (empty-scene (posn-x *dim*)
                                (posn-y *dim*)))
                  (field zombies))
           (field junk)))
  
  ;; Int Int Mouse -> World
  (define/public (on-mouse x y m)
    (cond [(mouse=? "button-down" m)
           (new world%
                (posn x y)
                (field zombies)
                (field junk) 
                (posn x y))]
          [(mouse=? "move" m)
           (new world%
                (field player)
                (field zombies)
                (field junk)
                (posn x y))]
          [else this]))

  (define/public (stop-when)
    (game-over?))
  
  ;; -> Boolean
  ;; Does the player touch zombies or junk?
  (define/public (game-over?)
    (ormap (λ (p) (touching? (field player) p))
           (append (field junk)
                   (field zombies))))
  
  ;; -> World
  ;; Move all the zombies toward the player.
  (define/public (move)
    (let ((p (field player)))
      (new world%
           (+ (field player) 
              (min-taxi 5
                        (field player)
                        (field mouse-posn)))
           (map (λ (r) (+ r (min-taxi 1 r p)))
                (field zombies))
           (field junk)
           (field mouse-posn))))

  ;; -> World
  ;; Junk all zombies that touch other zombies or junk.
  (define/public (junk-it)
    ;; maybe-zs are zombies if they don't touch any zs-to-consider.
    (local [(define (seg zs-to-consider maybe-zs junk)
              (cond [(empty? zs-to-consider) 
                     (new world%
                          (field player)
                          (reverse maybe-zs)
                          junk
                          (field mouse-posn))]
                    [else
                     (let ((touching-first? (λ (p) (touching? (first zs-to-consider) p))))
                       (cond [(ormap touching-first? (append maybe-zs junk))
                              (seg (rest zs-to-consider)
                                   (filter (λ (x) (not (touching-first? x))) maybe-zs)
                                   (cons (first zs-to-consider)
                                         (append (filter touching-first? maybe-zs)
                                                 junk)))]
                             [else
                              (seg (rest zs-to-consider)
                                   (cons (first zs-to-consider)
                                         maybe-zs)
                                   junk)]))]))]
      (seg (field zombies) empty (field junk)))))

;; Posn Posn -> Nat
;; Distance in taxicab geometry (domain knowledge).
(define (dist p1 p2)
  (posn-sum (posn-abs (- p1 p2))))

;; Nat Posn Posn -> Dir
;; Compute a direction that minimizes the distance between from and to.
(define (min-taxi x from to)
  ;; Dir Dir -> Dir
  (local [(define (select-shorter-dir d1 d2)
            (if (< (dist (+ from d1) to)
                   (dist (+ from d2) to))
                d1
                d2))]
    (foldl select-shorter-dir 0 DIRS)))



;; A Dir (Direction) is one of:
;; -1-1i -1 -1+1i 0-1i 0 0+1i 1-1i 1 1+1i
;; Interp: unit positions
(define DIRS 
  '(-1-1i -1 -1+1i 0-1i 0 0+1i 1-1i 1 1+1i))



(define *cell-size* 20)

;; Posn Posn -> Boolean
;; Are the points "touching"?
(define (touching? p1 p2)
  (<= (dist p1 p2) 
      (/ *cell-size* 2)))
  
;; Color -> Posn Scene -> Scene
;; Render the posn on the scene in the given color.
(define (posn+scn c) 
  (λ (p s)
    (place-image (circle (/ *cell-size* 2) "solid" c)
                 (posn-x p)
                 (posn-y p)
                 s)))   

;; Posn library
;; Nat Nat -> Posn
(define (posn x y)
  (+ x (* 0+1i y)))
(define posn-x real-part)
(define posn-y imag-part)
(define (posn-abs p) 
  (posn (abs (real-part p))
        (abs (imag-part p))))
(define (posn-sum p)
  (+ (posn-x p) (posn-y p)))
(define (random-posn p)
  (let ((r (random (* (real-part p) (imag-part p)))))
    (posn (quotient r (posn-y p))
          (remainder r (posn-y p)))))

(define *dim* (posn 400 400))

;; Run program, run!
(big-bang
 (new world%
      (posn 0 0)
      (build-list (+ 10 (random 20))
                  (λ (_)
                    (random-posn *dim*)))
      empty
      (posn 0 0)))