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

@title{Visitors and Folds}

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

@classblock{
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
}

We will add a single method to the interface for ranges:

@classblock{
;; The Range interface includes:
;; - visit : [RangeVisitor X] -> X
}

We haven't said what is in the @tt{[RangeVisitor X]} interface, but
the key idea is that something that implements a @tt{[RangeVisitor
X]} represents a computation over ranges that computes an @tt{X}.  So
for example, if we wanted to compute where a number is included in a
range, we would want to implement the @tt{[RangeVistitor Boolean]}
interface since that computation would produce a yes/no answer.

The idea, in general, of the visitor pattern is that the visitor will
have a method for each variant of a union.  And the method for a
particular variant takes as many arguments as there are fields in that
variant.  In the case of a recursive union, the method takes the
result of recursively visiting the data.

Under that guideline, the @tt{[RangeVisitor X]} interface will
contain 3 methods:

@classblock{
;; An [RangeVisitor X] implements:
;; lo-range : Number Number -> X
;; hi-range : Number Number -> X
;; union-range : X X -> X
}

Notice that the contracts for lo and hi-range match the contracts on
the constructors for each variant, but rather than constructing a
@tt{Range}, we are computing an @tt{X}.  In the case of
@racket[union-range], the method takes two inputs which are @tt{X}s, which
represent the results of visiting the left and right ranges,
respectively.

Now we need to implement the @racket[visit] method in each of the
@tt{Range} classes, which will just invoke the appropriate method of
the visitor on its data and recur where needed:

@classblock{
(define-class lo-range%
  (fields lo hi)
 
  (define (visit v)
    (v . lo-range (this . lo) (this . hi))))

(define-class hi-range%
  (fields lo hi)

  (define (visit v)
    (v . hi-range (this . lo) (this . hi))))

(define-class union-range%
  (fields left right)

  (define (visit v)
    (v . union-range (this . left . visit v) 
                     (this . right . visit v))))
}

We've now established the visitor pattern.  Let's actually construct a
visitor that does something.

We forgot to implement the @racket[in-range?] method, but no worries -- we
dont' need to edit our class definitions, we can just write a visitor
that does the @racket[in-range?] computation, which is an implementation
of @tt{[RangeVisitor Boolean]}:

@classblock{
;; An InRange? is an (in-range?% Number) 
;; implements [RangeVisitor Boolean].

(define-class in-range%?
  (fields n)

  (define (lo-range lo hi)
    (and (>= (this . n) lo)
         (<  (this . n) hi)))

  (define (hi-range lo hi)
    (and (>  (this . n) lo)
         (<= (this . n) hi)))

  (define (union-range left right)
    (or left right)))
}

Now if we have our hands on a range and want to find out if a number
is in the range, we just invoke the @racket[visit] method with an instance
of the @racket[in-range?%] class:

@classblock{
(some-range-value . visit (in-range?% 5))   ;; is 5 in some-range-value ?
}

@section{Folds}

[FIXME]

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
     (text (number->string (this . num)) 20 "black")
     (empty-scene 500 500)))
  ;; on-key : Key -> World
  (define (on-key k)
    (world% (this . generator)
            ((this . generator) . pick))))

;; A Generator is a (generator% [Listof Number])
;; and implements
;; pick : -> Number
;; produce a number to show that isn't in bad
(define-class generator%
  (fields bad)
  (define (pick)
    (local [(define x (random 10))]
      (cond [(member x (this . bad)) (pick)]
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
     (text (number->string (this . num)) 20 "black")
     (empty-scene 500 500)))
  ;; on-key : Key -> World
  (define (on-key k)
    (cond [(key=? k "x")
           (local [(define g (this . generator . add-bad (this . num)))]
             (world% g (g . pick)))]
          [else
           (world% (this . generator)
                   (this . generator . pick))])))

;; A Generator is a (generator% [Listof Number])
;; and implements
;; pick : -> Number
;; produce a number to show that isn't in bad
;; add-bad : Number -> Generator
;; produce a generator like that with an additional bad number
(define-class generator%
  (fields bad)
  (define (add-bad n)
    (generator% (cons n (this . bad))))
  (define (pick)
    (local [(define x (random 10))]
      (cond [(member x (this . bad)) (pick)]
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
     (text (number->string (this . num)) 20 "black")
     (empty-scene 500 500)))
  ;; on-key : Key -> World
  (define (on-key k)
    (cond [(key=? "x" k)
           (begin (this . generator . tell-bad)
                  (world% (this . generator)
                          (this . generator . pick)))]
          [else
           (world% (this . generator)
                   (this . generator . pick))])))

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
    (set-field! bad (cons (this . last) (this . bad))))
  (define (pick)
    (local [(define rnd (random 10))]
      (cond [(member rnd (this . bad)) (this . pick)]
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
     (text (number->string (this . num)) 20 "black")
     (empty-scene 500 500)))
  ;; on-key : Key -> World
  (define (on-key k)
    (cond [(key=? k "x")
           (world% (this . generator)
                   (this . generator . pick-bad))]
          [else
           (world% (this . generator)
                   (this . generator . pick))])))


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
    (begin (set-field! bad (cons (this . last) (this . bad)))
           (pick)))
  (define (pick)
    (local [(define x (random 10))]
      (cond [(member x (this . bad)) (this . pick)]
            [else (begin (set-field! last x)
                         x)]))))

(check-expect (<= 0 ((generator% empty 0) . pick) 10) true)
(check-expect (= ((generator% (list 4) 0) . pick) 4) false)

(big-bang (world% (generator% empty 0) 0))
}

@section{Exercises}

@subsection{Quick visits}

This problem builds on the @emph{quick lists} problem.

Here was the interface you should have implemented for lists using the
@emph{quick list} data structure that supports a fast
@racket[list-ref] method:

@codeblock{
;; A [List X] implements
;; - cons : X -> [List X]
;;   Cons given element on to this list.
;; - first : -> X
;;   Get the first element of this list 
;;   (only defined on non-empty lists).
;; - rest : -> [List X]
;;   Get the rest of this 
;;   (only defined on non-empty lists).
;; - list-ref : Natural -> X
;;   Get the ith element of this list 
;;   (only defined for lists of i+1 or more elements).
;; - length : -> Natural
;;   Compute the number of elements in this list.

;; empty is a [List X] for any X.
}

Make sure your quick list implementation is working and place it into
a file named @racket{quick-lists.rkt}.  That file should provide one
name, @racket[empty], by including the following at the top of the
file:

@codeblock{
(provide empty)
}

In a file named @racket{slow-lists.rkt} @emph{re-}develop an
implementation of the list interface, but in the usual way as a
recursive union of @racket[mt%] and @racket[cons%] classes.  That file
should also provide @racket[empty] by including the same line above at
the top.

Finally, start a third file called @racket{use-lists.rkt} that will
make use of both kinds of lists by including the following at the top
of the file:

@codeblock{
(require (prefix-in q: "quick-lists.rkt"))
(require (prefix-in s: "slow-lists.rkt"))
}

You now have two lists: @racket[q:empty] and @racket[s:empty];
both are represented in very different ways, but so long as you use
them accoring to the list interface, they should be indistinguishable.

Let's now revise the @tt{[List X]} interface to include support for
visitors:

@codeblock{
;; A [List X] implements ...
;; - accept : [ListVisitor X Y] -> Y
;;   Accept given visitor and visit this list's data.

;; A [ListVisitor X Y] implements
;; - visit-mt : -> Y
;;   Visit an empty list.
;; - visit-cons : X [Listof X] -> Y
;;   Visit a cons lists.
}

Implement the revised @tt{[List X]} interface in both
@racket{quick-lists.rkt} and @racket{slow-lists.rkt}.

In @racket{use-lists.rkt} you should be able to define particular
visitors and have it work on @emph{both} representations of lists.  As
an example, here is a list visitor that computes the length of a list:

@codeblock[#:keep-lang-line? #f]{
#lang class/2
;; A (new length%) implements [ListVisitor X Natural].
;; List visitor for computing the length of a list.
(define-class length%
  (define (visit-mt) 0)
  (define (visit-cons x r)
    (add1 (r . accept this))))

(define len (new length%))

(check-expect (q:empty . accept len) 0)
(check-expect (s:empty . accept len) 0)
(check-expect (q:empty . cons 'c . cons 'b . cons 'a . accept len) 3)
(check-expect (s:empty . cons 'c . cons 'b . cons 'a . accept len) 3)
}

And here's one for the sum of a list of numbers:

@codeblock[#:keep-lang-line? #f]{
#lang class/2
;; A (new sum%) implements [ListVisitor Number Number].
;; List visitor for computing the sum of a list of numbers.
(define-class sum%
  (define (visit-mt) 0)
  (define (visit-cons n r)
    (+ n (r . accept this))))

(define sum (new sum%))

(check-expect (q:empty . accept sum) 0)
(check-expect (s:empty . accept sum) 0)
(check-expect (q:empty . cons 3 . cons 4 . cons 7 . accept sum) 14)
(check-expect (s:empty . cons 3 . cons 4 . cons 7 . accept sum) 14)
}

Implement a @tt{[ListVisitor X X]} named @racket[reverse%] that
reverses a list (note: you may need to implement a ``helper'' visitor
that corresponds to the helper function you'd write for the
@racket[reverse] function).  Note that this visitor will have to
commit to produces either a quick list or a slow list, but it really
doesn't really matter which... well, except for testing.  So for
example, let's say the reverse visitor produces slow lists.  Then we
would expect the following test to pass, assuming @racket[reverse%]
works as specified:

@codeblock[#:keep-lang-line? #f]{
#lang class/2
(define rev (new reverse%))

(check-expect (q:empty . accept rev) s:empty)
(check-expect (q:empty . cons 'c . cons 'b . cons 'a . accept rev) 
              (s:empty . cons 'a . cons 'b . cons 'c))
}

Of course, this isn't ideal since our @emph{test} is testing more than
is actually required of @racket[reverse%].  In particular, it should
be perfectly acceptable for @racket[reverse%] to produce quick lists
without tests failing.

What's happening here is that @racket[check-expect] is checking too
much because it is not treating the objects it compares solely
according to their interface.  We will see how to fix this problem by
defining an interface-respecting equality computation, but for now,
just test as shown above.

Now to build some larger pieces with visitors.  First, here's an
interface definition for functional objects that represent functions
from @tt{X}s to @tt{Y}s.  Such an object has a single method called
@racket[apply] that consumes an @tt{X} and produces a @tt{Y}:

@codeblock[#:keep-lang-line? #f]{
#lang class/2
;; A [Fun X Y] implements
;; - apply : X -> Y
;;   Apply this function to given x.
}

Now implement the following two visitors:

@codeblock[#:keep-lang-line? #f]{
#lang class/2
;; A (new filter% [Predicate X]) implements [ListVisitor X [List X]].
;; Filters visited list to produce a list of elements satisfying predicate.

;; A (new map% [Fun X Y]) implements [ListVisitor X [List Y]].
;; Maps visited list to produce a list of results of applying the function.
}

Implement at least one @tt{[Fun Natural String]} and one @tt{[Predicate
String]} to use for testing @racket[filter%] and @racket[map%].


@subsection{Folds vs Visitors}

We can also implements @emph{folds} over lists, in both for both kinds
of lists.  Extend your implementation of lists (both kinds) to support
the @racket[fold] method:
@codeblock{
;; A [List X] implements ...
;; - fold : [ListFold X Y] -> Y
;;   Accept given fold and process this list's data.

;; A [ListFold X Y] implements
;; - fold-mt : -> Y
;;   Process an empty list.
;; - fold-cons : X Y -> Y
;;   Process a cons lists.
}

Now revise your implementations of the @racket[filter%] and
@racket[map%] to implement folds as well as visitors.  Be sure to
specify what interfaces they implement.

Finally, implement the class @racket[list-ref%]:
@codeblock[#:keep-lang-line? #f]{
#lang class/2
;; A (new list-ref% Number) implements [ListVisitor X X]
;; Retrieves the element at the specified index.
}

Could you implement this using the @racket[ListFold] interface?  Which
was more elegant for @racket[map%] and @racket[filter%]?

@subsection{JSON visitor}

Develop the visitor pattern for JSON values.

Design an equality visitor for JSON values.