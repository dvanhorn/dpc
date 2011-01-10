#lang scribble/manual
@(require scribble/eval
	  "../utils.rkt"
          racket/sandbox
          #;(for-label lang/htdp-intermediate-lambda)
  	  (for-label (only-in lang/htdp-intermediate-lambda define-struct))
          (for-label (except-in class0 define-struct)))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    ;(the-eval '(require lang/htdp-intermediate-lambda))
    (the-eval '(require class0))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    #|(call-in-sandbox-context 
     the-eval 
     (lambda () ((dynamic-require 'htdp/bsl/runtime 'configure)
                 (dynamic-require 'htdp/isl/lang/reader 'options))))|#
    the-eval))

@title[#:tag "lec01"]{1/10: Object = structure + functions}

@internal{

@bold{Outline}
@itemlist[
 @item{Announcements
   @itemlist[
     @item{Course staff introductions.}
     @item{Basic class mechanics.}
     @item{On the experimental nature of this course.}
     @item{The @seclink["assign01"]{first assignment}
           is due @bold{this} Wednesday.}
     @item{The @seclink["lab01"]{first lab} is @bold{tonight}.}
     @item{Partners have already been assigned.  See the @seclink["Blog"]{blog}.}
     @item{Questions?}]}
 @item{Basics of objects
   @itemlist[
     @item{New paradigm.  Open your mind and embrace it (or you will be miserable).}
     @item{Rocket, designed in functional style.}
     @item{Rocket, designed in object-oriented style.}
     @item{Landing and take off.}
     @item{Satellite.}]}]

@scheme-from-file["lectures/rocket-oo.rkt" #:start "lang" #:end "big-bang"]


@examples[#:eval the-eval
                 (bitmap "lectures/rocket.png")
                 (circle 10 "solid" "red")]
 
 



Rocket + Satellite.

[High-level: here's why objects are a good idea.  Motivated by world
programs.

Data + functionality, using atomic data (numbers) and compound data
(pair of numbers).  Methods for animation.]

Ch. 1, 2, 10


@bold{Objects} are an old programming concept that first appeared in the 1950s
across the Charles river.

As a first approximation, you can think of an @emph{object} as
a structure coupled together with the functions that operate 
on that structure.

@margin-note{Compared with 2500, we will use more precise data
             definitions for numbers.
             There are many kinds of numbers, including @tt{Natural},
             @tt{Integer}, @tt{Rational}, @tt{Real}, @tt{Complex},
             or the all-encompasing @tt{Number}.                          
             Here, we use @tt{Real} numbers, which are numbers
             for which the @racket[real?] predicate holds.}             
As an example, think about the following data definition for
representing coordinates on the Cartesian plane:
@racketblock[
  @code:comment{A Coord is a (make-coord Real Real).}]
@interaction[#:eval the-eval  
  (define-struct coord (x y))]

This generates the following template for functions that
consume @tt{Coord}s:
 @#reader scribble/comment-reader
   (racketblock   
   (define (coord-template p ...)
     (coord-x p) ... ;; Real
     (coord-y p))    ;; Real
   )

We can design the @racket[dist-to-origin] function that
computes the 
@link["http://en.wikipedia.org/wiki/Euclidean_distance"]{Euclidean distance} 
of a given @tt{Coord} to the origin; the Euclidean distance measures the
distance "as the crow flies": it is the length of a straight line between
the origin and the point.

@racketblock[
  @code:comment{Compute the distance from the given point to origin.}
  @code:comment{Coord -> Real}]
@defs+int[#:eval the-eval  
 ((check-expect (dist-to-origin (make-coord 0 0)) 0)
  (check-expect (dist-to-origin (make-coord 3 4)) 5)
  
  (define (dist-to-origin p)
    (sqrt (+ (sqr (coord-x p))
             (sqr (coord-y p))))))  
  
  (dist-to-origin (make-coord 0 0))
  (dist-to-origin (make-coord 3 4))]

Additionally, we can design the @racket[taxi-dist-to-origin] function
that computes the @link["http://en.wikipedia.org/wiki/Manhattan_distance"]{Manhattan distance} of a given @tt{Coord}
to the origin; the Manhattan distances measures the distance
"as the taxi drives": it is the length of any shortest
path between the origin and the point @emph{where you must
travel along a grid}.

@racketblock[
  @code:comment{Compute the taxicab distance from the given point to origin.}
  @code:comment{Coord -> Real}]
@defs+int[#:eval the-eval
 ((check-expect (taxi-dist-to-origin (make-coord 0 0)) 0)
  (check-expect (taxi-dist-to-origin (make-coord 3 4)) 7)
  
  (define (taxi-dist-to-origin p)
    (+ (abs (coord-x p))
       (abs (coord-y p)))))
 
 (taxi-dist-to-origin (make-coord 0 0))
 (taxi-dist-to-origin (make-coord 3 4))]
 
Finally, we can design the @racket[translate] function
that computes the translation of a given @tt{Coord} and
@racket[∂x] and @racket[∂y].

@racketblock[
  @code:comment{Translate the given point by ∂x, ∂y.}
  @code:comment{Coord Real Real -> Coord}]
@defs+int[#:eval the-eval
 ((check-expect (translate (make-coord 0 0) 1 2) (make-coord 1 2))
  (check-expect (translate (make-coord 3 4) -3 -4) (make-coord 0 0))
  
  (define (translate p ∂x ∂y)
    (make-coord (+ (coord-x p) ∂x)
                (+ (coord-y p) ∂y))))
 
 (translate (make-coord 0 0) 1 2)
 (translate (make-coord 3 4) -3 -4)]
             
At this point, we have defined a new class of data---the set of @tt{Coord}s---and
a few functions over that class of data.

@emph{Class}-oriented design can be seen as a way of organizing programs
that groups together the class of data and its computations.
An instance of this class of "data with computation" is an @emph{object}.

Let's demonstrate the idea by re-designing the coordinate data definition and
functions using a class-oriented design.

@racketblock[
  @code:comment{A Coord is a (new coord% Real Real).}]
@interaction[#:eval the-eval 
  (define-class coord%
    (fields x y))]

@margin-note{We will use the @tt{%}-suffix as a naming convention 
                             for any name that refers to a class.}
This is the @racket[coord%]-class
definition, which is analogous to the earlier @racket[coord]-structure definition.
Just like an instance of the @racket[coord] structure, an instance of the
@racket[coord%] class has an @racket[x] and a @racket[y] component.
The syntax for constructing and tearing apart @racket[coord%]s is slightly different
from strutures.  Rather than using the @racket[make-coord] constructor function,
a @racket[coord%] object is constructed with @racket[(new coord% 1 2)].

The way to invoke a computation on an object is to send the object a message it
understands.  The simplest computations on structured data are the accessor computations
that retrieve components of the data.  Just as structure definitions provide
accessors functions, class definitions provide objects with accessor @emph{methods}, 
which can be invoked by sending the field name as a message to the object.

@examples[#:eval the-eval
   (send (new coord% 1 2) x)
   (send (new coord% 1 2) y)
   (new coord% 1 (circle 10 "solid" "red"))]


}