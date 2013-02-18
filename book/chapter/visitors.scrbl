#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/1 check-expect define-struct ... length
                                numerator denominator))
          (for-label 2htdp/image)
          (for-label (only-in test-engine/racket-tests check-expect))
          (for-label class/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class/2))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@title{Visitors}

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
 
  (define (visit v)
    (v . lo-range (field lo) (field hi))))

(define-class hi-range%
  (fields lo hi)

  (define (visit v)
    (v . hi-range (field lo) (field hi))))

(define-class union-range%
  (fields left right)

  (define (visit v)
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

@codeblock{
#lang class/2
(require 2htdp/image class/universe)

;; A World is (world% Generator Number)
;; and implements IWorld
(define-class world%
  (fields generator num)
  ;; to-draw : -> Scene
  (define (to-draw)
    (overlay
     (text (number->string (field num)) 20 "black")
     (empty-scene 500 500)))
  ;; on-key : Key -> World
  (define (on-key k)
    (world% (field generator)
            ((field generator) . pick))))

;; A Generator is a (generator% [Listof Number])
;; and implements
;; pick : -> Number
;; produce a number to show that isn't in bad
(define-class generator%
  (fields bad)
  (define (pick)
    (local [(define x (random 10))]
      (cond [(member x (field bad)) (pick)]
            [else x]))))
(check-expect (<= 0 ((generator% empty) . pick) 10) true)
(check-expect (= ((generator% (list 4)) . pick) 4) false)

(big-bang (world% (generator% empty) 0))
}

@tt{generator-register.rkt}

@codeblock{
#lang class/2
(require 2htdp/image class/universe)

;; A World is (world% Generator Number)
;; and implements IWorld
(define-class world%
  (fields generator num)
  ;; to-draw : -> Scene
  (define (to-draw)
    (overlay
     (text (number->string (field num)) 20 "black")
     (empty-scene 500 500)))
  ;; on-key : Key -> World
  (define (on-key k)
    (cond [(key=? k "x")
           (local [(define g ((field generator) . add-bad (field num)))]
             (world% g (g . pick)))]
          [else
           (world% (field generator)
                   ((field generator) . pick))])))

;; A Generator is a (generator% [Listof Number])
;; and implements
;; pick : -> Number
;; produce a number to show that isn't in bad
;; add-bad : Number -> Generator
;; produce a generator like that with an additional bad number
(define-class generator%
  (fields bad)
  (define (add-bad n)
    (generator% (cons n (field bad))))
  (define (pick)
    (local [(define x (random 10))]
      (cond [(member x (field bad)) (pick)]
            [else x]))))
(check-expect (<= 0 ((generator% empty) . pick) 10) true)
(check-expect (= ((generator% (list 4)) . pick) 4) false)
(check-expect (= (((generator% empty) . add-bad 4) . pick) 4) false)

(big-bang (world% (generator% empty) 0))
}

@tt{generator-initial.rkt}

@codeblock{
#lang class/3
(require 2htdp/image class/universe)

;; A World is (world% Generator Number)
;; and implements IWorld
(define-class world%
  (fields generator num)
  ;; to-draw : -> Scene
  (define (to-draw)
    (overlay
     (text (number->string (field num)) 20 "black")
     (empty-scene 500 500)))
  ;; on-key : Key -> World
  (define (on-key k)
    (cond [(key=? "x" k)
           (begin ((field generator) . tell-bad)
                  (world% (field generator)
                          ((field generator) . pick)))]
          [else
           (world% (field generator)
                   ((field generator) . pick))])))

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
  (define (tell-bad)
    (set-field! bad (cons (field last) (field bad))))
  (define (pick)
    (local [(define rnd (random 10))]
      (cond [(member rnd (field bad)) (pick)]
            [else (begin
                    (set-field! last rnd)
                    rnd)]))))
(check-expect (<= 0 ((generator% (list 2 4 6) 0) . pick) 100) true)

(big-bang (world% (generator% (list 2 4 6) 0) 0))

(check-expect (member ((generator% (list 2 4 6) 0) . pick)
                      (list 2 4 6))
              false)

(define (tell-bad-prop g)
  (local [(define picked (g . pick))]
    (begin (g . tell-bad)
           (not (= picked (g . pick))))))

(check-expect (tell-bad-prop (generator% (list 1 2 3) 0)) true)
}

@tt{generator-mutate.rkt}

@codeblock{
#lang class/3
(require 2htdp/image class/universe)

;; A World is (world% Generator Number)
;; and implements IWorld
(define-class world%
  (fields generator num)
  ;; to-draw : -> Scene
  (define (to-draw)
    (overlay
     (text (number->string (field num)) 20 "black")
     (empty-scene 500 500)))
  ;; on-key : Key -> World
  (define (on-key k)
    (cond [(key=? k "x")
           (world% (field generator)
                   ((field generator) . pick-bad))]
          [else
           (world% (field generator)
                   ((field generator) . pick))])))
}

;; A Generator is a (generator% [Listof Number] Number)
;; interp: numbers not to pick, last number picked
;; and implements
;; pick : -> Number
;; produce a number to show that isn't in bad
;; pick-bad : -> Number
;; pick a number to show, and remember that the last one was bad
(define-class generator%
  (fields bad last)
  (define (pick-bad)
    (begin (set-field! bad (cons (field last) (field bad)))
           (pick)))
  (define (pick)
    (local [(define x (random 10))]
      (cond [(member x (field bad)) (pick)]
            [else (begin (set-field! last x)
                         x)]))))

(check-expect (<= 0 ((generator% empty 0) . pick) 10) true)
(check-expect (= ((generator% (list 4) 0) . pick) 4) false)

(big-bang (world% (generator% empty 0) 0))
)
