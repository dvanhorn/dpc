#lang racket
;; Orc Battle
;; You battle a bunch of monsters until you or they die.

;; Based on The Orc Battle Game, p. 172 of Barski's Land of Lisp.
(require racket/class)
(require 2htdp/image)
;; Design notes:
;; We use objects to represent players and monsters.
;; We try to disentangle object state, I/O, and computation
;; in Barski's design.  For example, consider Barski's method:
#|
(defmethod monster-hit (m x) 
  (decf (monster-health m) x)
  (if (monster-dead m) 
      (progn (princ "You killed the ")
             (princ (type-of m))
             (princ "! ")) 
      (progn (princ "You hit the ")
             (princ (type-of m))
             (princ ", knocking off ") 
             (princ x) 
             (princ " health points! "))))
|#
;; We model this with three methods:
;; * hit! - effect: decrements the monster's health.
;; * show-hit - computes: message describing the hit.
;; * display-hit - I/O: prints message describing the hit.

;; Isolating these elements facilitates testing and makes the
;; program easier to adapt, yet it is still easy to re-capture 
;; the conflated behavior by mixin if desired (see conflate-<%>).

(define obj% 
  (class* object% ()
    (inspect false)
    (super-new)
    (init-field health)   
    (define/public (dead?)
      (not (positive? health)))
    (define/public (hit! x)
      (set! health (- health x)))
     (define/public (display k)
      (k (send this show)))))

(define player% 
  (class* obj% ()
    (inspect false)
    (init [(the-health health) 30])    
    (super-new [health the-health])
    (init-field [agility 30] [strength 30])
    (inherit-field health)
    (define/public (dec-agility! x)
      (set! agility (- agility x)))
    (define/public (dec-strength! x)
      (set! strength (- strength x)))    
    (define/public (show)
      (string-append  "You are a valiant knight with a health of "
                      (number->string health)
                      ", an agility of "
                      (number->string agility)
                      ", and a strength of "
                      (number->string strength)
                      "."))))

(define monster%
  (class* obj% ()
    (inspect false)
    (init [(the-health health) (randval 10)])
    (super-new [health the-health])
    (inherit-field health)
    (define/public (type) "monster")
    (define/public (show)
      (string-append "A fierce " (send this type)))
    (define/public (show-hit x)
      (if (<= health x)
          (string-append "You killed the " (type) "!")
          (string-append "You hit the " (type) 
                         ", knocking off " 
                         (number->string x) 
                         " health points!")))
    (define/public (display-hit x k) 
      (k (send this show-hit x)))
    (define/public (display-attack p k)
      (k (send this show-attack p)))))

(define orc%
  (class* monster% ()
    (inspect false)
    (init-field [club-level (randval 8)])
    (field [attack-level (randval club-level)])    
    (super-new)
    (define/override (type) "orc")    

    (define/override (show)
      (string-append "A wicked orc with a level "
                     (number->string club-level)
                     " club."))
    
    (define/public (show-attack p)
      (string-append "An orc swings his club at you and knocks off "
                     (number->string attack-level)
                     " of your health points."))
        
    ;; Effect: decrement p's health and generate a new attack level.
    (define/public (attack! p)
      (send p hit! attack-level)
      (set! attack-level (randval club-level)))))

(define hydra%
  (class* monster% ()
    (inspect false)
    (super-new)
    (inherit-field health)
    (field [attack-level (randval (half health))])
    (define/override (type) "hydra")
    
    (define/override (show)
      (string-append "A malicious hydra with "
                     (number->string health)
                     " heads."))
    
    (define/public (show-attack p)
      (string-append "A hydra attacks you with "
                     (number->string attack-level)
                     " of its heads!  It also grows back one more head!"))
    
    (define/override (show-hit x)
      (if (<= health x)
          (string-append "The corpse of the fully decapitated and "
                         "decapacitated hydra falls to the floor!")
          (string-append "You lop off " 
                         (number->string x) 
                         " of the hydra's heads!")))
    
    (define/public (attack! p)      
      (send p hit! attack-level)
      (set! health (add1 health))
      (set! attack-level (randval (half health))))))

(define slime%
  (class* monster% ()
    (inspect false)
    (super-new)        
    (init-field [sliminess (randval 5)])
    (field [agil-attack (randval sliminess)])
    (field [squirt? (zero? (random 2))])
    (define/override (type) "slime mold")   
    
    (define/override (show)
      (string-append "A slime mold with a sliminess of "
                     (number->string sliminess)))
    
    (define/public (show-attack p)
      (string-append "A slime mold wraps around your legs and decreases your agility by "
                     (number->string agil-attack)
                     "!"
                     (if squirt?
                         " It also squirts in your face, taking away a health point!"
                         "")))
    
    (define/public (attack! p)
      (when squirt? (send p hit! 1))
      (send p dec-agility! agil-attack)
      (set! agil-attack (randval sliminess))
      (set! squirt? (zero? (random 2))))))

(define brigand%
  (class* monster% ()
    (inspect false)
    (super-new)    
    (define/override (type) "cunning brigand")    
    
    (define/public (show-attack p)
      (let ((x (max (get-field health p) 
                    (get-field agility p)
                    (get-field strength p))))
        (cond [(= x (get-field health p))               
               (string-append "A brigand hits you with his slingshot, "
                              "taking off 2 health points!")]
              [(= x (get-field agility p))
               (string-append "A brigand catches your leg with his whip, "
                              "taking off 2 agility points!")]                
              [else
               (string-append "A brigand cuts your arm with his whip, "
                              "taking off 2 strength points!")])))
    
    (define/public (attack! p)
      (let ((x (max (get-field health p) 
                    (get-field agility p)
                    (get-field strength p))))
        (cond [(= x (get-field health p))               
               (send p hit! 2)]
              [(= x (get-field agility p))
               (send p dec-agility! 2)]
              [else
               (send p dec-strength! 2)])))))

;; Nat -> Nat[1,n]
;; Effect: compute a random nat in [1,n].
(define (randval n)
  (add1 (random (max 1 n))))

;; Nat -> Nat
;; Computes (quotient n 2)
(define (half n)
  (arithmetic-shift n -1))

;; [Vector Monster] -> Boolean
;; Are all the monsters dead?
(define (dead? monsters)
  (for/and ([m (in-vector monsters)])
    (send m dead?)))

;; Re-tangle up effects, IO, and computation.
(define (conflate-<%> %)
  (class* % ()
    (super-new)
    (inherit show)
    (define/public (show!)
      (display (show)))))
(define (conflate-monster-<%> %)
  (class* (conflate-<%> %) ()
    (super-new)
    (inherit display-hit)
    (inherit display-attack)
    (define/override (hit! x)
      (display-hit x display)
      (newline)
      (super hit! x))
    (define/override (attack! p)
      (display-attack p display)
      (newline)
      (super attack! p))))

(define monster-classes
  (list orc% hydra% slime% brigand%))

;; CONSOLE GAME
(define (orc-battle!)
  (define monsters 
    (init-monsters (map conflate-monster-<%>
                        monster-classes)
                   12))
  (define p (new (conflate-<%> player%)))
  (game-loop! p monsters)
  (when (send p dead?)
    (display "You have been killed. Game Over."))
  (when (dead? monsters)
    (display "Congratulations!  You have vanquished all of your foes.")))

;; Player [Vector Monster] !
;; Effect: battle to the death.
(define (game-loop! p monsters)
  (let loop ()
    (unless (or (send p dead?)
                (dead? monsters))
      (send p show!)
      (for ([i (in-range (add1 (quotient (max 0 (get-field agility p)) 15)))])
        (unless (dead? monsters)
          (show monsters)
          (player-attack p monsters)))
      (newline)
      (for ([m (in-vector monsters)])
        (unless (send m dead?)
          (send m attack! p)))
      (loop))))
  
;; Player [Vector Monster] !
(define (player-attack p monsters)
  (newline)
  (display "Attack style: [s]tab [d]ouble swing [r]oundhouse: ")
  (let loop ()
    (case (read)
      [(s) (send (pick-monster monsters) hit! 
                 (+ 2 (randval (half (get-field strength p)))))]
      [(d) (let ((x (randval (quotient (get-field strength p) 6))))
             (display "Your double swing has a strength of ")
             (display x)
             (display ".")
             (newline)
             (send (pick-monster monsters) hit! x)
             (unless (dead? monsters)
               (send (pick-monster monsters) hit! x)))]
      [(r) (for ([i (in-range (add1 (randval (quotient (get-field strength p) 3))))])
             (unless (dead? monsters)
               (send (random-living monsters) hit! 1)))]
      [else (loop)])))
       
;; [Vector Monster] -> Monster
;; Effect: prompt the user to select a living monster.
(define (pick-monster monsters)
  (newline)
  (display "Monster #: ")
  (let ((x (read)))
    (if (not (and (integer? x) (<= 0 x (sub1 (vector-length monsters)))))
        (begin (display "That is not a valid monster number.")
               (pick-monster monsters))
        (let ((m (vector-ref monsters x)))
          (if (send m dead?)
              (begin (display "That monster is dead.")
                     (pick-monster monsters))
              m)))))
        
;; [Vector Monster] -> Monster
;; Effect: print a listing of monsters.
(define (show monsters)
  (newline)
  (display "Your foes:")
  (for ([m (in-vector monsters)]
        [i (in-range (vector-length monsters))])
    (newline)
    (display "   ")
    (display i)
    (display ". ")
    (if (send m dead?) 
        (display "**dead**")
        (begin (display "(Health=")
               (display (get-field health m))
               (display ") ")
               (send m show!)))))
  
;; [Listof [-> Monster]] Nat -> [Vector Monster]
;; Effect: randomly generate monsters.
(define (init-monsters builders num)
  (build-vector num (λ (_) (new (random-pick builders)))))
  
;; [Vector Obj] -> Obj
;; Randomly pick a living object.
;; Asssume: there is a living obj in os.
(define (random-living os)
  (vector-ref os (random-pick (living-indices os))))

;; [Vector Obj] -> [Listof Nat]
;; Asssume: there is a living obj in os.
(define (living-indices os)
  (for/fold ([living-index empty])
    ([i (in-range (vector-length os))]
     [o (in-vector os)])
    (if (send o dead?)
        living-index
        (cons i living-index))))

;; (cons X [Listof X]) -> X
;; Effect: randomly pick an element.
(define (random-pick ls)
  (list-ref ls (random (length ls))))               
