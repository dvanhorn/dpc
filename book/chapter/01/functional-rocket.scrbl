#lang scribble/manual
@(require scribble/eval
          class/utils
          racket/sandbox
          (for-label (except-in lang/htdp-intermediate-lambda image?))
          (for-label 2htdp/image)
          (for-label 2htdp/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    ;(the-eval '(require lang/htdp-intermediate-lambda))
    (the-eval '(require class/0))
    (the-eval '(require 2htdp/image))
    ;(the-eval '(require (prefix-in r: racket)))
    #|(call-in-sandbox-context 
     the-eval 
     (lambda () ((dynamic-require 'htdp/bsl/runtime 'configure)
                 (dynamic-require 'htdp/isl/lang/reader 'options))))|#
    the-eval))  

@(the-eval
  `(begin     
     ;; A World is a Rocket.
     
     ;; A Rocket is a non-negative Number.
     ;; Interp: distance from the ground to base of rocket in AU.
     
     (define CLOCK-SPEED  1/30) ; SEC/TICK
     (define ROCKET-SPEED 1)    ; AU/SEC
     
     (define ROCKET (bitmap ,(string-append (path->string (collection-path "class/0")) "/rocket.png"))) ;; Use rocket key to insert the rocket here.
     (define WIDTH 100)   ;; PX
     (define HEIGHT 200)  ;; PX
     (define MT-SCENE (empty-scene WIDTH HEIGHT))
     
     ;; next : Rocket -> Rocket
     ;; Compute next position of the rocket after one tick of time.
     (check-expect (next 10) (+ 10 DELTA))
     (define (next r)
       (+ r DELTA))
     
     ;; render : Rocket -> Scene
     ;; Render the rocket as a scene.
     (define (render r)
       (draw-on r MT-SCENE))
     
     ;; draw-on : Rocket Scene -> Scene
     ;; Draw rocket on to scene.
     (check-expect (draw-on 0 (empty-scene 100 100))
                   (overlay/align/offset "center" "bottom"
                                         ROCKET
                                         0 1
                                         (empty-scene 100 100)))
     (define (draw-on r scn)
       (overlay/align/offset "center" "bottom" 
                             ROCKET 
                             0 (add1 r)
                             scn))))
     
@title{Functional rocket}

In this section, let's develop a simple program that animates the
lift-off of a @as-index{rocket}.

The animation will be carried out by using the
@as-index{@racket[big-bang]} system of the
@as-index{@racketmodname[2htdp/universe]} library.  For an
@as-index{animation}, @racket[big-bang] requires settling on a
representation of @deftech{world states} and two functions: one that
renders a world state as an image, and one that consumes a world state
and produce the subsequent world state.

Generically speaking, to make an animation we must design a program of
the form: @index*['("on-tick") (list @racket[on-tick])]{}
@index*['("to-draw") (list @racket[to-draw])]{}

@classblock{
(big-bang <world0>           ; World
          (on-tick <tick>)   ; World -> World
          (to-draw <draw>))  ; World -> Scene
}

where @racket[World] is a data definition for world states,
@racket[<tick>] is an expression whose value is a @racket[World ->
World] function that computes successive worlds and @racket[<draw>] is
an expression whose value is a @racket[World -> Scene] function that
renders a world state as an image.

For the purposes of a simple animation, the world state can consist of
just the rocket:

@classblock{
;; A World is a Rocket.
}

The only relevant piece of information that we need to keep track of
to represent a rocket lifting off is its height.  That leads us to
using a single number to represent rockets.  Since rockets only go up
in our simple model, we can use non-negative numbers.  We'll interpret
a non-negative number as meaning the distance between the ground and
the (base of the) rocket measured in
@link["http://en.wikipedia.org/wiki/Astronomical_unit"]{@deftech{astronomical
units}} (@index*['("AU" "astronomical units") (list "AU"
@tech{astronomical units})]{AU}):

@classblock{
;; A Rocket is a non-negative Number.
;; Interp: distance from the ground to base of rocket in AU.
}

This dictates that we need to develop two functions that consume
@tt{Rocket}s:

@classblock{
;; next : Rocket -> Rocket
;; Compute next position of the rocket after one tick of time.

;; render : Rocket -> Scene
;; Render the rocket as a scene.
}

Let's take them each in turn.  

@section{The @racket[next] function}

@index*['("next") (list @racket[next])]{}

For @index*['("rocket" "next") (list "rocket"
@racket[next])]{@racket[next]}, in order to compute the next position
of a rocket we need to settle on the amount of elapsed time a call to
@racket[next] embodies and how fast the rocket rises per unit of time.
For both, we define constants:

@classblock{
(define CLOCK-SPEED  1/30) ; SEC/TICK
(define ROCKET-SPEED 1)    ; AU/SEC
}

The @index*['("rocket" "CLOCK-SPEED") (list "rocket"
@racket[CLOCK-SPEED])]{@racket[CLOCK-SPEED]} is the rate at which the
clock ticks, given in seconds per tick, and @index*['("rocket"
"ROCKET-SPEED") (list "rocket"
@racket[ROCKET-SPEED])]{@racket[ROCKET-SPEED]} is the rate at which
the rocket lifts off, given in @as-index{AU} per second.  We use these
two constants to define a third, computed, constant that gives change
in the rocket's distance from the ground per clock tick:

@classblock{
(define DELTA (* CLOCK-SPEED ROCKET-SPEED)) ; AU/TICK
}

@index*['("rocket" "DELTA") (list "rocket" @racket[DELTA])]{}

We can now give examples of how @racket[next] should work.  We are
careful to write test-cases in terms of the defined constants so that
if we revise them later our tests will still be correct:

@classblock{
(check-expect (next 10) (+ 10 DELTA))
}

Now that we have develop a purpose statement, contract, and example,
we can write the code, which is made clear from the example:

@classblock{
;; next : Rocket -> Rocket
;; Compute next position of the rocket after one tick of time.
(check-expect (next 10) (+ 10 DELTA))
(define (next r)
  (+ r DELTA))
}

@section{The @racket[render] function}

@index*['("render") (list @racket[render])]{}

The purpose of @index*['("render") (list
@racket[render])]@racket[render] is visualize a rocket a scene.
Remember that rockets are represented by the distance between the
ground and their base, so a rocket at height @racket[0] should sitting
at the bottom of a scene.  We want it to look something like:

@interaction[#:eval the-eval
(render 0)
]

To do so we need to settle on the size of the sceen and the look of
the rocket.  Again, we define constants for this.  We use the
@as-index{@racketmodname[2htdp/image]} library for constructing
@as-index{images}.

@#reader scribble/comment-reader
(racketblock
(define ROCKET #,(image (string-append (path->string (collection-path "class/0")) "/rocket.png"))) ;; Use rocket key to insert the rocket here.
(define WIDTH 100)   ;; PX
(define HEIGHT 200)  ;; PX
(define MT-SCENE (empty-scene WIDTH HEIGHT))
)

@index*['("ROCKET") 
(list (image (string-append (path->string (collection-path "class/0")) "/rocket.png")))]{}
@index*['("ROCKET") (list @racket[ROCKET])]{}

@index*['("empty-scene") (list @racket[empty-scene])]{}

You can copy and paste the rocket image from this program, or
you can access the image as follows:

@interaction[#:eval the-eval
(bitmap class/0/rocket.png)
]

@index*['("bitmap") (list @racket[bitmap])]{}

Since we may want to draw rockets on scenes other than the
@racket[MT-SCENE], let's develop a helper function:

@classblock{
;; draw-on : Rocket Scene -> Scene
;; Draw rocket on to scene.
(define (draw-on r scn) ...)
}

@index*['("draw-on") (list @racket[draw-on])]{}

allowing us to define @racket[render] simpy as:

@classblock{
;; render : Rocket -> Scene
;; Render the rocket as a scene.
(define (render r)
  (draw-on r MT-SCENE))
}

Recall that a rocket is represented by the distance from the ground to
its @bold{base}.  On the other hand, the
@as-index{@racket[2htdp/image]} library works in terms of
@deftech{pixels} (@index*['("PX" "pixels") (list "PX"
@tech{pixels})]{PX}) and @deftech{graphics coordinates}.  We need
@racket[draw-on] to establish the mapping between @as-index{AU}.  For
simplicity, we assume 1 PX equals 1 AU.  Using
@racket[overlay/align/offset], the @racket[draw-on] functions places
the rocket on the scene on the center, bottom of the scene, offset
vertically by the height of the rocket:

@classblock{
;; draw-on : Rocket Scene -> Scene
;; Draw rocket on to scene.
(define (draw-on r scn)
  (overlay/align/offset "center" "bottom" 
                        ROCKET 
                        0 (add1 r)
                        scn))
}

@index*['("overlay/align/offset") (list @racket[overlay/align/offset])]{}

@section{Lift off}

With these functions in place, let's @index['("rocket" "launch")]{launch a rocket}:

@classblock{
;; Lift off!
(big-bang 0
          (tick-rate CLOCK-SPEED)
          (on-tick next)
          (to-draw render))
}

@index*['("big-bang") (list @racket[big-bang])]{}
@index*['("tick-rate") (list @racket[tick-rate])]{}
@index*['("on-tick") (list @racket[on-tick])]{}
@index*['("to-draw") (list @racket[to-draw])]{}

Our complete BSL program is:

@#reader scribble/comment-reader
(racketblock
  (require 2htdp/image)
  (require 2htdp/universe)

  ;; A World is a Rocket.

  ;; A Rocket is a non-negative Number.
  ;; Interp: distance from the ground to base of rocket in AU.

  (define CLOCK-SPEED  1/30) ; SEC/TICK
  (define ROCKET-SPEED 1)    ; AU/SEC
  (define DELTA (* CLOCK-SPEED ROCKET-SPEED)) ; AU/TICK
  
  (define ROCKET #,(image (string-append (path->string (collection-path "class/0")) "/rocket.png"))) ;; Use rocket key to insert the rocket here.
  (define WIDTH 100)   ;; PX
  (define HEIGHT 200)  ;; PX
  (define MT-SCENE (empty-scene WIDTH HEIGHT))
  
  ;; next : Rocket -> Rocket
  ;; Compute next position of the rocket after one tick of time.
  (check-expect (next 10) (+ 10 DELTA))
  (define (next r)
    (+ r DELTA))
  
  ;; render : Rocket -> Scene
  ;; Render the rocket as a scene.
  (define (render r)
    (draw-on r MT-SCENE))
  
  ;; draw-on : Rocket Scene -> Scene
  ;; Draw rocket on to scene.
  (check-expect (draw-on 0 (empty-scene 100 100))
                (overlay/align/offset "center" "bottom"
                                      ROCKET
                                      0 1
                                      (empty-scene 100 100)))
  (define (draw-on r scn)
    (overlay/align/offset "center" "bottom" 
                          ROCKET 
                          0 (add1 r)
                          scn))
  
  ;; Lift off!
  (big-bang 0
            (tick-rate CLOCK-SPEED)
            (on-tick next)
            (to-draw render))
  
)

