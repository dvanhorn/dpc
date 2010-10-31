#lang racket
;; The Wizard's Adventure Game!
;; All zombies move towards the player.  Zombies collision cause flesh heaps
;; that are deadly to other zombies (and you!).  Teleport as a last resort!

;; Based on The Wizard's Adventure Game, p. 68 of Barski's Land of Lisp.
(require 2htdp/image)
(require 2htdp/universe)

;; He arrived without shoes *and* he suggesting representing
;; text with symbols...
;; Unbelievable.

(define (conn->string c) (symbol->string c))
(define (loc->string c)  (symbol->string c))
(define (dir->string c)  (symbol->string c))

;; A Conn is (U 'ladder 'door).
;; A Loc is (U 'living-room 'garden 'attic).
;; A Dir is (U 'upstairs 'downstairs 'east 'west).
;; An Edge is a [List Loc Dir Conn].

(define (describe-room loc)
  (foldr (λ (e s)
           (cond [(eq? (second e) 'west)
                  (beside (scale 1/4 (room-scene (first e))) s)]
                 [(eq? (second e) 'east)
                  (beside s (scale 1/4 (room-scene (first e))))]
                 [(eq? (second e) 'upstairs)
                  (above (scale 1/4 (room-scene (first e))) s)]
                 [(eq? (second e) 'downstairs)
                  (above s (scale 1/4 (room-scene (first e))))]))
         (room-scene loc)
         (rest (assoc loc *edges*))))

(define (room-scene loc)
  (cond [(symbol=? loc 'living-room) living-room-scene]
        [(symbol=? loc 'garden) garden-scene]
        [(symbol=? loc 'attic) attic-scene]))
  
(define living-room-scene
  (overlay (above (text "WIZARD'S LIVING ROOM" 40 "white")
                  (text "A wizard is snoring loudly on the couch." 20 "white"))
           (square 500 "solid" "purple")))

(define garden-scene
  (overlay (above (text "GARDEN" 100 "white")
                  (text "There is a well in front of you." 20 "white"))
           (square 500 "solid" "green")))

(define attic-scene
  (overlay (above (text "ATTIC" 100 "white")
                  (text "There is a giant welding torch in the corner." 20 "white"))
           (square 500 "solid" "black")))

(define *edges* 
  '((living-room (garden west door) 
                 (attic upstairs ladder))
    (garden (living-room east door)) 
    (attic (living-room downstairs ladder))))

;; Loc Dir -> Loc
;; Try to move in the given direction.
(define (try-change-room loc dir)
  (let ((r (memf (λ (e) (eq? dir (second e)))
                 (rest (assoc loc *edges*)))))
    (if r (first (first r)) loc)))

(big-bang 'living-room
          (on-draw (λ (r) (overlay (describe-room r)
                                   (square 800 "solid" "white"))))
          (on-key (λ (r k)
                    (cond [(key=? "up" k)
                           (try-change-room r 'upstairs)]
                          [(key=? "down" k)
                           (try-change-room r 'downstairs)]
                          [(key=? "left" k)
                           (try-change-room r 'west)]
                          [(key=? "right" k)
                           (try-change-room r 'east)]
                          [else r]))))
