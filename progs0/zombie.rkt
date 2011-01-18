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

(define CELL-SIZE 20)
(define WIDTH 400)
(define HEIGHT 400)
(define MT-SCENE (empty-scene WIDTH HEIGHT))

;; A World is a (world Posn LoP LoP Posn).
;; An LoP is a [Listof Posn].
;; A Posn is a (make-posn Int Int).

(define-class world%
  (fields player live dead mouse)
  
  (define/public (name)
    "Zombie Attack!")
  
  (define/public (on-tick)
    (send (junk-it) move))
  
  (define/public (tick-rate)
    1/10)    
  
  ;; -> Scene
  (define/public (to-draw)    
    (foldr (posn+scn "red") 
           ((posn+scn "green")
            (field player)
            (foldr (posn+scn "gray")
                   MT-SCENE
                   (field dead)))
           (field live)))
  
  ;; Int Int Mouse -> World
  (define/public (on-mouse x y m)
    (cond [(mouse=? "button-down" m)
           (new world%
                (make-posn (random WIDTH)
                           (random HEIGHT))
                (field live)
                (field dead) 
                (make-posn x y))]
          [(mouse=? "move" m)
           (new world%
                (field player)
                (field live)
                (field dead)
                (make-posn x y))]
          [else this]))
  
  (define/public (stop-when)
    (game-over?))
  
  ;; -> Boolean
  ;; Does the player touch zombies or junk?
  (define/public (game-over?)
    (ormap (λ (p) (touching? (field player) p))
           (append (field dead)
                   (field live))))
  
  ;; -> World
  ;; Move all the zombies toward the player.
  (define/public (move)
    (new world%
         (posn+ (field player) 
                (min-taxi 5
                          (field player)
                          (field mouse)))
         (map (λ (r) 
                (posn+ r (min-taxi 1 r (field player))))
              (field live))
         (field dead)
         (field mouse)))
  
  ;; -> World
  ;; Junk all zombies that touch other zombies or junk.
  (define/public (junk-it)                                    
    (let ((res (kill (field live) (field dead))))
      (new world% 
           (field player)
           (r-live res)
           (r-dead res)
           (field mouse)))))

;; LoP LoP -> (make-r LoP LoP)
;; Kill any live zombies and move to dead list.
(define-struct r (live dead))
(define (kill live dead)
  (cond [(empty? live)
         (make-r empty dead)]                    
        [else
         (local [(define z (first live))]
           (cond [(or (ormap (λ (ml) (touching? z ml))
                             (rest live))
                      (ormap (λ (dd) (touching? z dd))
                             dead))
                  (kill (rest live)
                        (cons z dead))]
                 [else
                  (let ((res (kill (rest live)
                                   dead)))
                    (make-r (cons z (r-live res)) 
                            (r-dead res)))]))]))

;; Posn Posn -> Nat
;; Distance in taxicab geometry (domain knowledge).
(define (dist p1 p2)
  (+ (abs (- (posn-x p1)
             (posn-x p2)))
     (abs (- (posn-y p1)
             (posn-y p2)))))

;; Nat Posn Posn -> Dir
;; Find direction to minimizes distance between from and to.
(define (min-taxi x from to)
  ;; Dir Dir -> Dir
  (local [(define (select-shorter-dir d1 d2)
            (if (< (dist (posn+ from d1) to)
                   (dist (posn+ from d2) to))
                d1
                d2))]
    (foldl (λ (d sd) 
             (select-shorter-dir sd 
                                 (make-posn (* x (posn-x d)) 
                                            (* x (posn-y d)))))
           (make-posn 0 0)
           DIRS)))

;; A Dir (Direction) is one of:
(define DIRS
  (list (make-posn -1 -1)
        (make-posn -1  0)
        (make-posn -1 +1)
        (make-posn  0 -1)
        (make-posn  0  0)
        (make-posn  0 +1)
        (make-posn +1 -1)
        (make-posn +1  0)
        (make-posn +1 +1)))

;; Posn Posn -> Boolean
;; Are the points "touching"?
(define (touching? p1 p2)
  (<= (dist p1 p2) 
      (/ CELL-SIZE 2)))

;; Color -> Posn Scene -> Scene
;; Render the posn on the scene in the given color.
(define (posn+scn c) 
  (λ (p s)
    (place-image (circle (/ CELL-SIZE 2) "solid" c)
                 (posn-x p)
                 (posn-y p)
                 s)))   

;; Posn Posn -> Posn
(define (posn+ p1 p2) 
  (make-posn (+ (posn-x p1) (posn-x p2))
             (+ (posn-y p1) (posn-y p2))))

;; Run program, run!
(big-bang
 (new world%
      (make-posn 0 0)
      (build-list (+ 10 (random 20))
                  (λ (_)
                    (make-posn (random WIDTH) 
                               (random HEIGHT))))
      empty
      (make-posn 0 0)))
