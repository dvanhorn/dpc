#lang scribble/manual
@(require scribble/eval
	  "../web/utils.rkt"
          racket/sandbox
          #;(for-label lang/htdp-intermediate-lambda)
  	  (for-label (only-in lang/htdp-intermediate-lambda define-struct))
          (for-label (except-in class0 define-struct))
	  (for-label class0/universe))

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

@title[#:tag "lec01"]{Object = structure + functions}


@section{Functional rocket}

Here's a review of the first program we wrote last semester.  It's the
program to launch a rocket, written in the Beginner Student Language:

@racketblock[
(require 2htdp/image)
(require 2htdp/universe)

@code:comment{Use the rocket key to insert the rocket here.}
(define ROCKET (bitmap class0/rocket.png))

(define WIDTH 100)
(define HEIGHT 300)
(define MT-SCENE (empty-scene WIDTH HEIGHT))

@code:comment{A World is a Number.}
@code:comment{Interp: distance from the ground in AU.}

@code:comment{render : World -> Scene}
(check-expect (render 0) 
	      (place-image ROCKET (/ WIDTH 2) HEIGHT MT-SCENE))
(define (render h)
  (place-image ROCKET
               (/ WIDTH 2)
               (- HEIGHT h)
               MT-SCENE))

@code:comment{next : World -> World}
(check-expect (next 0) 7)
(define (next h)
  (+ h 7))

(big-bang 0
          (on-tick next)
          (to-draw render))
]

It's a shortcoming of our documentation system that we can't define
@racket[ROCKET] to be the rocket image directly, but as you can see
this did the right thing:

@(the-eval '(define ROCKET (bitmap class0/rocket.png)))
@(the-eval '(define WIDTH 100))
@(the-eval '(define HEIGHT 300))
@(the-eval '(define MT-SCENE (empty-scene WIDTH HEIGHT)))
@(the-eval
'(define-class world% 
  (fields height)
  (define/public (on-tick)
    (new world% (add1 (field height))))
  (define/public (to-draw)
    (place-image ROCKET
    		 (/ WIDTH 2)
		 (- HEIGHT (field height))
		 MT-SCENE))))

@interaction[#:eval the-eval
ROCKET
]

You can use the same trick, or you can copy this rocket image and
paste it directly into DrRacket.

@section{Object-oriented rocket}

You'll notice that there are two significant components to this
program.  There is the @emph{data}, which in this case is a number
representing the height of the rocket, and the @emph{functions} that
operate over that class of data, in this case @racket[next] and
@racket[render].

This should be old-hat programming by now.  But in this class, we are
going to explore a new programming paradigm that is based on
@emph{objects}.  Objects are an old programming concept that first
appeared in the 1950s just across the Charles river.  As a first
approximation, you can think of an @emph{object} as the coupling
together of the two significant components of our program (data and
functions) into a single entity: an object.

Since we are learning a new programming language, you will no longer
be using BSL and friends.  Instead, select
@menuitem["Language"]{Choose Language...} in DrRacket, then select the
"Use the language declared in the source" option and add the following
to the top of your program:

@racketmod[class0]

The way to define a class is with @racket[define-class]:
@racketblock[
(define-class world%
  (fields height))
]

This declares a new class of values, namely @racket[world%] objects.
(By convention, we will use the @tt{%} suffix for the
name of classes.)  For the moment, @racket[world%] objects consist
only of data: they have one @emph{field}, the @racket[height] of the
rocket.

Like a structure definition, this class definition defines a new kind
of data, but it does not make any particular instance of that data.
To make an new instance of a particular class, you use the
@racket[new] syntax, which takes a class name and expressions that
produce a value for each field of the new object.  Since a
@racket[world%] has one field, @racket[new] takes the shape:
@interaction[#:eval the-eval
(new world% 7)]

This creates a @racket[world%] representing a rocket with height @racket[7].

In order to access the data, we can invoke the @racket[height]
accessor method.  Methods are like functions for objects and they are
called (or @emph{invoked}) by using the @racket[send] form like so: 

@interaction[#:eval the-eval
(send (new world% 7) height)]

This suggests that we can now re-write the data definition for Worlds:
@racketblock[
@code:comment{A World is a (new world% Number).}
@code:comment{Interp: height represents distance from the ground in AU.}
]

To add functionality to our class, we define @emph{methods} using the
@racket[define/public] form.  In this case, we want to add two methods
@racket[on-tick] and @racket[to-draw]: 

@racketblock[ 
@code:comment{A World is a (new world% Number).}  
@code:comment{Interp: height represents distance from the ground in AU.}
(define-class world% 
  (fields height)

  @code:comment{on-tick : ...}
  (define/public (on-tick ...) ...)

  @code:comment{to-draw : ...}
  (define/public (to-draw ...) ...))
]

We will return to the contracts and code, but now that we've seen how
to define methods, let's look at how to @emph{invoke} them in order to
actually compute something.  To call a defined method, we again use
the @racket[send] form, which takes an object, a method name, and any
inputs to the method:

@racketblock[
(send (new world% 7) on-tick ...)
]

This will call the @racket[on-tick] method of the object created with
@racket[(new world% 7)].  This is analogous to calling the
@racket[next] function on the world @racket[7].  The ellided code
(@racket[...]) is where we would write additional inputs to the
method, but it's not clear what further inputs are needed, so now
let's turn to the contract and method signatures for @racket[on-tick]
and @racket[to-draw].

When we designed the functional analogues of these methods, the
functions took as input the world on which they operated, i.e. they
had contracts like:

@racketblock[
@code:comment{tick : World -> World}
@code:comment{render : World -> Scene}]

But in an object, the data and functions are packaged together.
Consequently, the method does not need to take the world input; that
data is already a part of the object and the values of the fields are
accessible using the @racket[field] form.

That leads us to the following method signatures:

@racketblock[
@code:comment{A World is a (new world% Number).}  
@code:comment{Interp: height represents distance from the ground in AU.}
(define-class world% 
  (fields height)

  @code:comment{on-tick : -> World}
  (define/public (on-tick) ...)

  @code:comment{to-draw : -> Scene}
  (define/public (to-draw) ...))
]

Since we have contracts and have seen how to invoke methods, we can
now formulate test cases:

@racketblock[
(check-expect (send (new world% 7) on-tick)
	      (new world% 8))
(check-expect (send (new world% 7) to-draw)
	      (place-image ROCKET
	      		   (/ WIDTH 2)
			   (- HEIGHT 7)
			   MT-SCENE))
]

Finally, we can write the code from our methods:

@racketblock[
@code:comment{A World is a (new world% Number).}  
@code:comment{Interp: height represents distance from the ground in AU.}
(define-class world% 
  (fields height)

  @code:comment{on-tick : -> World}
  (define/public (on-tick)
    (new world% (add1 (field height))))

  @code:comment{to-draw : -> Scene}
  (define/public (to-draw)
    (place-image ROCKET
    		 (/ WIDTH 2)
		 (- HEIGHT (field height))
		 MT-SCENE)))]

At this point, we can construct @racket[world%] objects and invoke
methods.

@examples[#:eval the-eval
(new world% 7)
(send (new world% 7) on-tick)
(send (new world% 80) to-draw)
]

But if we want to see an animation, we need to have our program
interact with the @racket[big-bang] system.  Since we are now writing
programs in an object-oriented style, we have a new @racket[big-bang]
system that uses an interface more suited to objects.  To import this
OO-style @racket[big-bang], add the following to the top of your
program:

@racketblock[
(require class0/universe)]

In the functional setting, we had to explicitly give a piece of data
representing the state of the initial world and list which functions
should be used for each event in the system.  In other words, we had
to give both data and functions to the @racket[big-bang] system.  In
an object-oriented system, the data and functions are already packaged
together, and thus the @racket[big-bang] form takes a single argument:
an object that both represents the initial world and implements the
methods needed to handle system events such as @racket[to-draw] and
@racket[on-tick].  (Our choice of method names was important; had we
used the names @racket[render] and @racket[next], for example,
@racket[big-bang] would not have known how to make the animation
work.)

So to launch our rocket, we simply do the following:

@racketblock[
(big-bang (new world% 0))]

Our complete program is:

@racketmod[
class0
(require 2htdp/image)
(require class0/universe)

@code:comment{Use the rocket key to insert the rocket here.}
(define ROCKET (bitmap class0/rocket.png))

(define WIDTH 100)
(define HEIGHT 300)
(define MT-SCENE (empty-scene WIDTH HEIGHT))

@code:comment{A World is a (new world% Number).}  
@code:comment{Interp: height represents distance from the ground in AU.}
(define-class world% 
  (fields height)

  @code:comment{on-tick : -> World}
  (define/public (on-tick)
    (new world% (add1 (field height))))

  @code:comment{to-draw : -> Scene}
  (define/public (to-draw)
    (place-image ROCKET
    		 (/ WIDTH 2)
		 (- HEIGHT (field height))
		 MT-SCENE)))

(check-expect (send (new world% 7) on-tick)
	      (new world% 8))
(check-expect (send (new world% 7) to-draw)
	      (place-image ROCKET
	      		   (/ WIDTH 2)
			   (- HEIGHT 7)
			   MT-SCENE))

@code:comment{Run, program, run!}
(big-bang (new world% 0))]

@section{Landing and taking off}

Let's now revise our program so that the rocket first descends toward
the ground, lands, then lifts off again.  Our current representation
of a world is insufficient since it's ambiguous whether we are going
up or down.  For example, if the rocket is at @racket[42], are we
landing or taking off?  There's no way to know.  We can revise our
data definition to included a representation of this missing
information.  A simple solution is to add a number representing the
velocity of the rocket.  When it's negative, we are moving toward the
ground.  When positive, we are taking off.

In the functional approach, this new design criterion motivated the
use of compound data, which are represented with structures.  With
objects, atomic and compound data are represented the same way; all
that changes are the number of fields contained in each object.

Our revised class definition is then:

@(the-eval
'(define-class world%
  (fields height velocity)))

@racketblock[
;; A World is a (new world% Number Number).
;; Interp: height represents the height of the rocket in AU,
;; velocity represents the speed of the rocket in AU/seconds.
(define-class world%
  (fields height velocity)
  ...)]

When constructing @racket[world%] objects now give two arguments in
addition to the class name:

@interaction[#:eval the-eval
(new world% 7 -1)]

Both components can be accessed through the accessor methods:

@interaction[#:eval the-eval
(send (new world% 7 -1) height)
(send (new world% 7 -1) velocity)]

And within method definitions, we can refer to the values of fields
using the @racket[field] form.  The signatures for our methods don't
change, and rockets should render just the same as before, however we
need a new behavior for @racket[on-tick].  When a world ticks, we want
its new height to be calculated based on its current height and
velocity, and its new velocity is the same as the old velocity, unless
the rocket has landed, in which case we want to flip the direction of
the velocity.

First, let's make some test cases for the new @racket[on-tick]:

@racketblock[
(check-expect (send (new world% 0 1) on-tick)
              (new world% 1 1))
(check-expect (send (new world% 10 -2) on-tick)
              (new world% 8 -2))
(check-expect (send (new world% 0 -1) on-tick)
              (new world% -1 1))
]

Based on this specification, we revise the @racket[on-tick] method
definition as follows:

@racketblock[
(define-class world%
  ...
  ;; on-tick : -> World 
  (define/public (on-tick)
    (new world%
         (+ (field velocity) (field height))
         (cond [(<= (field height) 0) (abs (field velocity))]
               [else (field velocity)])))
  ...)]

Giving us an overall program of:

@#reader scribble/comment-reader
(racketmod
class0
(require 2htdp/image)
(require class0/universe)

(define ROCKET (bitmap class0/rocket.png))

(define WIDTH 100)
(define HEIGHT 300)
(define MT-SCENE (empty-scene WIDTH HEIGHT))

;; A World is a (new world% Number Number).
;; Interp: height represents the height of the rocket in AU,
;; velocity represents the speed of the rocket in AU/seconds.
(define-class world%
  (fields height velocity)
  
  ;; on-tick : -> World 
  (define/public (on-tick)
    (new world%
         (+ (field velocity) (field height))
         (cond [(<= (field height) 0) (abs (field velocity))]
               [else (field velocity)])))
  
  ;; to-draw : -> Scene
  (define/public (to-draw)
    (place-image ROCKET
                 (/ WIDTH 2) 
                 (- HEIGHT (field height))
                 (empty-scene WIDTH HEIGHT))))

(check-expect (send (new world% 0 1) to-draw)
              (place-image ROCKET 
                           (/ WIDTH 2) 
                           HEIGHT 
                           MT-SCENE))

(check-expect (send (new world% 0 1) on-tick)
              (new world% 1 1))
(check-expect (send (new world% 10 -2) on-tick)
              (new world% 8 -2))
(check-expect (send (new world% 0 -1) on-tick)
              (new world% -1 1))

(big-bang (new world% HEIGHT -1)))

@section{Adding a moon}

Adding more components is straightforward.

Here is a complete program that includes an orbiting moon:

@#reader scribble/comment-reader
(racketmod
class0
(require 2htdp/image)
(require class0/universe)

(define ROCKET (bitmap class0/rocket.png))
(define MOON (circle 20 "solid" "blue"))

(define WIDTH 300)
(define HEIGHT 300)
(define MT-SCENE (empty-scene WIDTH HEIGHT))

; A World is a (new world% Number Number Number).
; Interp: height represents distance from the ground in AU.
; velocity represents the speed of the rocket in AU/sec.
; moon-height represents the height of the moon.
(define-class world%
  (fields height velocity moon-height)
  
  ;; on-tick : -> World 
  (define/public (on-tick)
    (new world%
         (+ (field velocity) (field height))
         (cond [(= (field height) 0) (abs (field velocity))]
               [else (field velocity)])
         (modulo (+ 5 (field moon-height)) 200)))
  
  ;; to-draw : -> Scene
  (define/public (to-draw)
    (place-image MOON
                 (/ WIDTH 3) 
                 (field moon-height)
                 (place-image ROCKET
                              (/ WIDTH 2) (- HEIGHT (field height))
                              MT-SCENE))))

(check-expect (send (new world% 0 1 100) to-draw)
              (place-image MOON
                           (/ WIDTH 3) 100
                           (place-image ROCKET (/ WIDTH 2) HEIGHT MT-SCENE)))

(check-expect (send (new world% 0 1 100) on-tick)
              (new world% 1 1 105))
(check-expect (send (send (new world% 10 -2 100) on-tick) height) 8)

(big-bang (new world% HEIGHT -1 100))
)


@internal{

@;scheme-from-file["lectures/rocket-oo.rkt" #:start "lang" #:end "big-bang"]

Rocket + Satellite.

[High-level: here's why objects are a good idea.  Motivated by world
programs.

Data + functionality, using atomic data (numbers) and compound data
(pair of numbers).  Methods for animation.]

Ch. 1, 2, 10



@margin-note{Compared with 2500, we will use more precise data
             definitions for numbers.
             There are many kinds of numbers, including @tt{Number},
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
