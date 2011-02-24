#lang scribble/manual
@(require "../utils.rkt"
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class1 check-expect define-struct ... length
				numerator denominator))
	  (for-label 2htdp/image)
	  (for-label (only-in test-engine/racket-tests check-expect))
	  (for-label class1/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class2))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@title[#:tag "lec14"]{2/24: Visitors}

Here is a high-level outline of what we covered in class today. Below is the
code we wrote. These notes will be fleshed out soon---we're posting them in this
unfinished form now so they can help you work on your assignment.

@verbatim|{

  Number generation game

    Want to remember and avoid generating numbers rejected by player
      Store in generator

    Why does `pick' terminate?
      You could stay unlucky forever...
        We'll avoid the delicious subtleties of this issue and move on

    sudo read xkcd

    With mutation
      Q: Is mutation bad design?
        Functional design is easier to test
        But mutation can sometimes simplify our programs

    Testing randomness is hard
    Testing mutation is hard

    Question Dan's attitude
    Question Dan's entropy

    Change `tell-bad : Number ->' to `tell-bad : ->'

    Property testing: as the number of inputs goes down, the amount of state
    stored in the generator goes up, along with the complexity of the test

    How do we apply the number generator game to the homework?
      Telling the number generator ``bad'' is a simple model of how we could
      tell our AI player not to repeat the last set of decisions it made.

  Visitor pattern

    You already know it...

    Add one general method to your classes, `visit'
    Support many externally defined operations
    Robust against changes in representation
      e.g. 3union-range

    ...it's a fold

    Folds and visitors apply to more than just lists, and more than just
    functional data

}|

@tt{generator-bad.rkt}

@#reader scribble/comment-reader
(racketmod
class2
(require 2htdp/image class2/universe)

;; A World is (world% Generator Number)
;; and implements IWorld
(define-class world%
  (fields generator num)
  ;; to-draw : -> Scene
  (define/public (to-draw)
    (overlay
     (text (number->string (field num)) 20 "black")
     (empty-scene 500 500)))
  ;; on-key : Key -> World
  (define/public (on-key k)
    (world% (field generator)
            ((field generator) #,dot pick))))

;; A Generator is a (generator% [Listof Number])
;; and implements
;; pick : -> Number
;; produce a number to show that isn't in bad
(define-class generator%
  (fields bad)
  (define/public (pick)
    (local [(define x (random 10))]
      (cond [(member x (field bad)) (pick)]
            [else x]))))
(check-expect (<= 0 ((generator% empty) #,dot pick) 10) true)
(check-expect (= ((generator% (list 4)) #,dot pick) 4) false)

(big-bang (world% (generator% empty) 0))
)

@tt{generator-register.rkt}

@#reader scribble/comment-reader
(racketmod
class2
(require 2htdp/image class2/universe)

;; A World is (world% Generator Number)
;; and implements IWorld
(define-class world%
  (fields generator num)
  ;; to-draw : -> Scene
  (define/public (to-draw)
    (overlay
     (text (number->string (field num)) 20 "black")
     (empty-scene 500 500)))
  ;; on-key : Key -> World
  (define/public (on-key k)
    (cond [(key=? k "x")
           (local [(define g ((field generator) #,dot add-bad (field num)))]
             (world% g (g #,dot pick)))]
          [else
           (world% (field generator)
                   ((field generator) #,dot pick))])))

;; A Generator is a (generator% [Listof Number])
;; and implements
;; pick : -> Number
;; produce a number to show that isn't in bad
;; add-bad : Number -> Generator
;; produce a generator like that with an additional bad number
(define-class generator%
  (fields bad)
  (define/public (add-bad n)
    (generator% (cons n (field bad))))
  (define/public (pick)
    (local [(define x (random 10))]
      (cond [(member x (field bad)) (pick)]
            [else x]))))
(check-expect (<= 0 ((generator% empty) #,dot pick) 10) true)
(check-expect (= ((generator% (list 4)) #,dot pick) 4) false)
(check-expect (= (((generator% empty) #,dot add-bad 4) #,dot pick) 4) false)

(big-bang (world% (generator% empty) 0))
)

@tt{generator-initial.rkt}

@#reader scribble/comment-reader
(racketmod
class3
(require 2htdp/image class3/universe)

;; A World is (world% Generator Number)
;; and implements IWorld
(define-class world%
  (fields generator num)
  ;; to-draw : -> Scene
  (define/public (to-draw)
    (overlay
     (text (number->string (field num)) 20 "black")
     (empty-scene 500 500)))
  ;; on-key : Key -> World
  (define/public (on-key k)
    (cond [(key=? "x" k)
           (begin ((field generator) #,dot tell-bad)
                  (world% (field generator)
                          ((field generator) #,dot pick)))]
          [else
           (world% (field generator)
                   ((field generator) #,dot pick))])))

;; A Generator is a (generator% [Listof Number] Number)
;; interp: the list of bad numbers, and the last number picked
;; and implements
;; pick : -> Number
;; produce a number to show not in the list
;; tell-bad : ->
;; produces nothing
;; effect : changes the generator to add the last number picked to the bad list
(define-class generator%
  (fields bad last)
  (define/public (tell-bad)
    (set-field! bad (cons (field last) (field bad))))
  (define/public (pick)
    (local [(define rnd (random 10))]
      (cond [(member rnd (field bad)) (pick)]
            [else (begin
                    (set-field! last rnd)
                    rnd)]))))
(check-expect (<= 0 ((generator% (list 2 4 6) 0) #,dot pick) 100) true)

(big-bang (world% (generator% (list 2 4 6) 0) 0))

(check-expect (member ((generator% (list 2 4 6) 0) #,dot pick)
                      (list 2 4 6))
              false)

(define (tell-bad-prop g)
  (local [(define picked (g #,dot pick))]
    (begin (g #,dot tell-bad)
           (not (= picked (g #,dot pick))))))

(check-expect (tell-bad-prop (generator% (list 1 2 3) 0)) true)
)

@tt{generator-mutate.rkt}

@#reader scribble/comment-reader
(racketmod
class3
(require 2htdp/image class3/universe)

;; A World is (world% Generator Number)
;; and implements IWorld
(define-class world%
  (fields generator num)
  ;; to-draw : -> Scene
  (define/public (to-draw)
    (overlay
     (text (number->string (field num)) 20 "black")
     (empty-scene 500 500)))
  ;; on-key : Key -> World
  (define/public (on-key k)
    (cond [(key=? k "x")
           (world% (field generator)
                   ((field generator) #,dot pick-bad))]
          [else
           (world% (field generator)
                   ((field generator) #,dot pick))])))

;; A Generator is a (generator% [Listof Number] Number)
;; interp: numbers not to pick, last number picked
;; and implements
;; pick : -> Number
;; produce a number to show that isn't in bad
;; pick-bad : -> Number
;; pick a number to show, and remember that the last one was bad
(define-class generator%
  (fields bad last)
  (define/public (pick-bad)
    (begin (set-field! bad (cons (field last) (field bad)))
           (pick)))
  (define/public (pick)
    (local [(define x (random 10))]
      (cond [(member x (field bad)) (pick)]
            [else (begin (set-field! last x)
                         x)]))))

(check-expect (<= 0 ((generator% empty 0) #,dot pick) 10) true)
(check-expect (= ((generator% (list 4) 0) #,dot pick) 4) false)

(big-bang (world% (generator% empty 0) 0))
)
