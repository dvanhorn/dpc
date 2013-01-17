#lang scribble/manual
@(require scribble/eval
          class/utils
          racket/sandbox
          (for-label (except-in class/0 check-expect))
          (for-label (only-in lang/htdp-intermediate-lambda check-expect))
          (for-label class/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class/0))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@(the-eval 
  `(begin 
     (define CLOCK-SPEED  1/30) ; SEC/TICK
     (define ROCKET-SPEED 1)    ; AU/SEC
     (define DELTA (* CLOCK-SPEED ROCKET-SPEED)) ; AU/TICK

     (define ROCKET (bitmap ,(string-append (path->string (collection-path "class/0")) "/rocket.png")))
     (define WIDTH 100)
     (define HEIGHT 200)
     (define MT-SCENE (empty-scene WIDTH HEIGHT))
     (define-class rocket% 
       (fields dist)
       ;; next : -> Rocket
       (define (next)
         (new rocket% 
           (+ (send this dist) DELTA)))
       
    ;; render : -> Scene
    (define (render)
      (send this draw-on MT-SCENE))

    ;; draw-on : Scene -> Scene
    ;; Draw this rocket on to scene.
    (define (draw-on scn)
      (overlay/align/offset "center" "bottom" 
                            ROCKET 
                            0 (add1 (send this dist))
                            scn)))))    

@title{Object-oriented rocket}

Now let's redevelop this program only instead of using data and
functions, we'll use objects.

You'll notice that there are two significant components to the rocket
program.  There is the @emph{data}, which in this case is a number
representing the distance the rocket has traveled, and the
@emph{functions} that operate over that class of data, in this case
@racket[next] and @racket[render].

This should be old-hat programming by now.  But in this book, we are
going to explore a new programming paradigm that is based on
@emph{objects}.  As a first approximation, you can think of an
@emph{object} as the coupling together of the two significant
components of our program (data and functions) into a single entity:
an object.

Since we are learning a new programming language, you will no longer
be using BSL and friends.  Instead, select
@menuitem["Language"]{Choose Language...} in DrRacket, then select the
``Use the language declared in the source'' option and add the following
to the top of your program:

@racketmod[class/0]

@index*['("class/0") (list @racketmodname[class/0])]{}

The constants of the rocket program remain the same, so our new
program still includes a set of constant definitions:

@#reader scribble/comment-reader
(racketblock
 (define CLOCK-SPEED  1/30) ; SEC/TICK
 (define ROCKET-SPEED 1)    ; AU/SEC
 (define DELTA (* CLOCK-SPEED ROCKET-SPEED)) ; AU/TICK
 
 (define ROCKET #,(image (string-append (path->string (collection-path "class/0")) "/rocket.png"))) ;; Use rocket key to insert the rocket here.
 (define WIDTH 100)   ;; PX
 (define HEIGHT 200)  ;; PX
 (define MT-SCENE (empty-scene WIDTH HEIGHT)))

A set of objects is defined by a @deftech{class}, which determines the
number and name of fields and the name and meaning of each behavior
that every object is the set contains.  By analogy, while an object is
like a structure, a class definition is like a structure definition.

@section{A class of rockets}

The way to define a class is with @as-index{@racket[define-class]}:

@classblock{
(define-class rocket%
  (fields dist))
}

@index*['("rocket" "rocket%") (list "rocket" @racket[rocket%])]{}

This declares a new class of values, namely
@as-index{@racket[rocket%]} objects.  (By convention, we will use the
@tt{%} suffix for the name of classes.)  For the moment,
@racket[rocket%] objects consist only of data: they have one
@deftech{field}, the @racket[dist] between the rocket and the ground.

Like a structure definition, this class definition defines a new kind
of data, but it does not make any particular instance of that data.
To make a new instance of a particular class, i.e. an object, you use
the @racket[new] syntax, which takes a class name and expressions that
produce a value for each field of the new object.  Since a
@racket[rocket%] has one field, @racket[new] takes the shape:
@interaction[#:eval the-eval
(new rocket% 7)]

This creates a @racket[rocket%] representing a rocket with height
@racket[7].

In order to access the data, we can invoke the
@as-index{@racket[dist]} accessor method.  Methods are like functions
for objects and they are called by using the @as-index{@racket[send]}
form like so:

@interaction[#:eval the-eval
(send (new rocket% 7) dist)]

This suggests that we can now re-write the data definition for
@tt{Rocket}s: 

@classblock{
  ;; A Rocket is a (new rocket% NonNegativeNumber)
  ;; Interp: distance from the ground to base of rocket in AU.
}

@section{The @racket[next] and @racket[render] methods}

To add functionality to our class, we define @deftech{methods} using
the @as-index{@racket[define] form}.  In this case, we want to add two
methods @as-index{@racket[next]} and @as-index{@racket[render]}:

@classblock{
  ;; A Rocket is a (new rocket% NonNegativeNumber)
  ;; Interp: distance from the ground to base of rocket in AU.
  (define-class rocket% 
    (fields dist)

    ;; next : ...
    (define (next ...) ...)

    ;; render : ...
    (define (render ...) ...))
}

We will return to the contracts and code, but now that we've seen how
to define methods, let's look at how to @emph{apply} them in order to
actually compute something.  To call a defined method, we again use
the @racket[send] form, which takes an object, a method name, and any
arguments to the method:

@classblock{
(send (new rocket% 7) next ...)
}

This will call the @racket[next] method of the object created with
@racket[(new rocket% 7)].  This is analogous to applying the
@racket[next] function to @racket[7] in the @secref{Functional_rocket}
section.  The elided code
(@racket[...]) is where we would write additional inputs to the
method, but it's not clear what further inputs are needed, so now
let's turn to the contract and method headers for @racket[next]
and @racket[render].

When we designed the functional analogues of these methods, the
functions took as input the rocket on which they operated, i.e. they
had headers like:

@classblock{
  ;; next : Rocket -> Rocket
  ;; Compute next position of the rocket after one tick of time.

  ;; render : Rocket -> Scene
  ;; Render the rocket as a scene.
}

But in an object, the data and functions are packaged together.
Consequently, the method does not need to take the world input; that
data is already a part of the object and the values of the fields are
accessible using accessors.  In other words, methods have an implicit
input that does not show up in their header---it is the object that
has called the method.  That value, since it is not available as an
explicit parameter of the method, is made available through the
@as-index{@racket[this] variable}.  We likewise revise the purpose
statements to reflect the fact ``the rocket'' is the object calling
the method, so we instead write ``this rocket'', emphasizing that
@racket[this] refers to a rocket.

That leads us to the following @index['("method" "headers")]{method
headers}:

@filebox[@racket[rocket%]]{
@classblock{
  ;; next : -> Rocket
  ;; Compute next position of this rocket after one tick of time.
  (define (next) ...)
  
  ;; render : -> Scene
  ;; Render this rocket as a scene.
  (define (render) ...)
}}

The @racket[rocket%] box is our way of saying that this code should
live in the @racket[rocket%] class.

Since we now have contracts and have seen how to invoke methods, we
can now formulate test cases:


@filebox[@racket[rocket%]]{
@classblock{
  ;; next : -> Rocket
  ;; Compute next position of this rocket after one tick of time.
  (check-expect (send (new rocket% 10) next)
                (new rocket% (+ 10 DELTA)))
  (define (next) ...)
  
  ;; render : -> Scene
  (check-expect (send (new rocket% 0) render)
                (overlay/align/offset "center" "bottom"
                                      ROCKET
                                      0 1
                                      MT-SCENE))
  (define (render) ...)
 }}

Finally, we can write the code from our methods:

@filebox[@racket[rocket%]]{
@classblock{
  (define (next)
    (new rocket% (+ (send this dist) DELTA)))
  
  (define (render)
    (send this draw-on MT-SCENE))
}}

Just as in the functional design, we choose to defer to a helper to
draw a rocket on to the empty scene, which we develop as the following
method:

@filebox[@racket[rocket%]]{
@classblock{
  ;; draw-on : Scene -> Scene
  ;; Draw this rocket on to scene.
  (define (draw-on scn)
    (overlay/align/offset "center" "bottom" 
                          ROCKET 
                          0 (add1 (send this dist))
                          scn))
}}

@index*['("overlay/align/offset") (list @racket[overlay/align/offset])]{}

At this point, we can construct @racket[rocket%] objects and invoke
methods.

@examples[#:eval the-eval
(new rocket% 7)
(send (new rocket% 7) next)
(send (new rocket% 80) render)
]

@section{A big-bang oriented to objects}

@(define-syntax-rule (def-racket big-bang-id)
  (begin
    (require (for-label (only-in 2htdp/universe big-bang)))
    (define big-bang-id (racket big-bang))))
@(def-racket big-bang-id)

It's now fairly easy to construct a program using @racket[rocket%]
objects that is of the generic form of a @as-index{@|big-bang-id|}
animation:

@#reader scribble/comment-reader
(racketblock
(#,big-bang-id <world0>           ; World
          (on-tick <tick>)   ; World -> World
          (to-draw <draw>))  ; World -> Scene
)

We can again define a world as a rocket:

@classblock{
;; A World is a Rocket.
}

We now need to construct @racket[Rocket -> Rocket] and @racket[Rocket
-> Scene] functions---but the work of these functions is already taken
care of by the @racket[next] and @racket[render] methods.  Thus we
construct simple functions that call the appropriate method on the
given rocket:
@#reader scribble/comment-reader
(racketblock
(require 2htdp/universe)
(#,big-bang-id (new rocket% 0)
          (on-tick (λ (r) (send r next)))
          (to-draw (λ (r) (send r render))))
)

This creates the desired animation, but something should stick out
about the above code.  The @|big-bang-id| system works by giving a
piece of data (a number, a position, an image, an object, etc.), and a
set of functions that operate on that kind of data.  That sounds a lot
like... @emph{an object!}  It's almost as if the interface for
@|big-bang-id| were designed for, but had to fake, objects.

Now that we have objects proper, we can use a new @racket[big-bang]
system has an interface more suited to objects.  To import this
OO-style @as-index{@racket[big-bang]}, add the following to the top of
your program:

@classblock{
(require class/universe)
}

@index*['("class/universe") (list @racketmodname[class/universe])]{}

In the functional setting, we had to explicitly give a piece of data
representing the state of the initial world and list which functions
should be used for each event in the system.  In other words, we had
to give both data and functions to the @racket[big-bang] system.  In
an object-oriented system, the data and functions are already packaged
together, and thus the @racket[big-bang] form takes a single argument:
an object that both represents the initial world and implements the
methods needed to handle system events such as @racket[to-draw] and
@racket[on-tick].

So to launch our rocket, we simply do the following:

@racketblock[
(big-bang (new rocket% 0))]

In order to handle events, we need to add the methods
@index*['("on-tick" "method") (list @racket[on-tick]
"method")]{@racket[on-tick]} and @index*['("to-draw" "method") (list
@racket[to-draw] "method")]{@racket[to-draw]} to @racket[rocket%]:

@index*['("rocket%" "on-tick") (list @racket[racket%] @racket[on-tick])]{}
@index*['("rocket%" "to-draw") (list @racket[racket%] @racket[to-draw])]{}

@filebox[@racket[rocket%]]{
@classblock{
  ;; on-tick : -> World
  ;; Tick this world
  (define (on-tick) ...)

  ;; to-draw : -> Scene
  ;; Draw this world
  (define (to-draw) ...)
}}

These methods, for the moment, are synonymous with @racket[next] and
@racket[render], so their code is simple:

@filebox[@racket[rocket%]]{
@classblock{
  (define (on-tick) (send this next))
  (define (to-draw) (send this render))
}}

Our complete program is:

@#reader scribble/comment-reader
(racketmod
  class/0
  (require 2htdp/image)
  (require class/universe)

  ;; A World is a Rocket.

  ;; A Rocket is a (new rocket% NonNegativeNumber).
  ;; Interp: distance from the ground to base of rocket in AU.

  (define CLOCK-SPEED  1/30) ; SEC/TICK
  (define ROCKET-SPEED 1)    ; AU/SEC
  (define DELTA (* CLOCK-SPEED ROCKET-SPEED)) ; AU/TICK
  
  (define ROCKET #,(image (string-append (path->string (collection-path "class/0")) "/rocket.png"))) ;; Use rocket key to insert the rocket here.
  (define WIDTH 100)   ;; PX
  (define HEIGHT 200)  ;; PX
  (define MT-SCENE (empty-scene WIDTH HEIGHT))
  
  (define-class rocket% 
    (fields dist)

    ;; next : -> Rocket
    ;; Compute next position of this rocket after one tick of time.
    (check-expect (send (new rocket% 10) next)
                  (new rocket% (+ 10 DELTA)))
    (define (next) 
      (new rocket% (+ (send this dist) DELTA)))
    
    ;; render : -> Scene
    ;; Render this rocket as a scene.
    (check-expect (send (new rocket% 0) render)
                  (overlay/align/offset "center" "bottom"
                                        ROCKET
                                        0 1
                                        MT-SCENE))
    (define (render) 
      (send this draw-on MT-SCENE))

    ;; draw-on : Scene -> Scene
    ;; Draw this rocket on to scene.
    (define (draw-on scn)
      (overlay/align/offset "center" "bottom"
                            ROCKET
                            0 (add1 (send this dist))
                            scn))    

    ;; on-tick : -> World
    ;; Tick this world
    (define (on-tick) (send this next))

    ;; to-draw : -> Scene
    ;; Draw this world   
    (define (to-draw) (send this render)))  
  
  ;; Lift off!
  (big-bang (new rocket% 0))
)

You've now seen the basics of how to write programs with objects.