#lang scribble/manual
@(require "../web/utils.rkt"
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

@title[#:tag "lec14"]{Visitors}

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

}|

@section{The Visitor Pattern}

The visitor pattern is a general design pattern that allows you to
seperate data from functionality in an object-oriented style.

For instance, suppose you want to develop a library of ranges.  Users
of your library are going to want a bunch of different methods, and in
principal, you can't possibly know or want to implement all of them.
On the other hand, you may not want to expose the implementation
details of @emph{how} you chose to represent ranges.

The visitor pattern can help---it requires you to implement @emph{one}
method which accepts what we call a "visitor" that is then exposed to
a view of the data.  Any computation over shapes can be implemented as
a visitor, so this one method is universal---no matter how people want
to use your library, this one method is enough to ensure they can
write whatever computation they want.  What's better is that even if
you change the representation of ranges, so long as you provide the
same "view" of the data, everything will continue to work.

So here is our data definition for ranges:

@verbatim|{
; A Range is one of
;; - (new lo-range% Number Number)
;; Interp: represents the range between `lo' and `hi'
;;         including `lo', but *not* including `hi'

;; - (new hi-range% Number Number)
;; Interp: represents the range between `lo' and `hi'
;;         including `hi', but *not* including `lo'

;; - (new union-range% Range Range)
;; Interp: including all the numbers in both ranges

(define-class lo-range%
  (fields lo hi))

(define-class hi-range%
  (fields lo hi))

(define-class union-range%
  (fields left right))
}|

We will add a single method to the interface for ranges:

@verbatim|{
;; The IRange interface includes:
;; - visit : [IRangeVisitor X] -> X
}|

We haven't said what is in the @tt{[IRangeVisitor X]} interface, but
the key idea is that something that implements a @tt{[IRangeVisitor
X]} represents a computation over ranges that computes an @tt{X}.  So
for example, if we wanted to compute where a number is included in a
range, we would want to implement the @tt{[IRangeVistitor Boolean]}
interface since that computation would produce a yes/no answer.

The idea, in general, of the visitor pattern is that the visitor will
have a method for each variant of a union.  And the method for a
particular variant takes as many arguments as there are fields in that
variant.  In the case of a recursive union, the method takes the
result of recursively visiting the data.

Under that guideline, the @tt{[IRangeVisitor X]} interface will
contain 3 methods:

@verbatim|{
;; An [IRangeVisitor X] implements:
;; lo-range : Number Number -> X
;; hi-range : Number Number -> X
;; union-range : X X -> X
}|

Notice that the contracts for lo and hi-range match the contracts on
the constructors for each variant, but rather than constructing a
@tt{Range}, we are computing an @tt{X}.  In the case of
@tt{union-range}, the method takes two inputs which are @tt{X}s, which
represent the results of visiting the left and right ranges,
respectively.

Now we need to implement the @tt{visit} method in each of the
@tt{Range} classes, which will just invoke the appropriate method of
the visitor on its data and recur where needed:

@verbatim|{
(define-class lo-range%
  (fields lo hi)
 
  (define/public (visit v)
    (v . lo-range (field lo) (field hi))))

(define-class hi-range%
  (fields lo hi)

  (define/public (visit v)
    (v . hi-range (field lo) (field hi))))

(define-class union-range%
  (fields left right)

  (define/public (visit v)
    (v . union-range ((field left) . visit v) 
                     ((field right) . visit v))))
}|

We've now established the visitor pattern.  Let's actually construct a
visitor that does something.

We forgot to implement the @tt{in-range?} method, but no worries -- we
dont' need to edit our class definitions, we can just write a visitor
that does the @tt{in-range?} computation, which is an implementation
of @tt{[IRangeVisitor Boolean]}:

@verbatim|{
;; An InRange? is an (in-range?% Number) 
;; implements [IRangeVisitor Boolean].

(define-class in-range%?
  (field n)

  (define (lo-range lo hi)
    (and (>= (field n) lo)
         (<  (field n) hi)))

  (define (hi-range lo hi)
    (and (>  (field n) lo)
         (<= (field n) hi)))
    
  (define (union-range left right)
    (or left right)))
}|

Now if we have our hands on a range and want to find out if a number
is in the range, we just invoke the @tt{visit} method with an instance
of the @tt{in-range?%} class:

@verbatim|{
(some-range-value . visit (in-range?% 5))   ;; is 5 in some-range-value ?
}|



@section{Generators}

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
