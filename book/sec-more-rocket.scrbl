#lang scribble/manual
@(require scribble/eval
	  class/utils
          (for-label (except-in class/0 check-expect))
	  (for-label (only-in lang/htdp-intermediate-lambda check-expect))
	  (for-label class/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class/0))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@title{Revisiting the Rocket}

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
'(define-class rocket%
  (fields height velocity)))

@racketblock[
;; A World is a (new rocket% Number Number).
;; Interp: height represents the height of the rocket in AU,
;; velocity represents the speed of the rocket in AU/seconds.
(define-class rocket%
  (fields height velocity)
  ...)]

When constructing @racket[rocket%] objects now give two arguments in
addition to the class name:

@interaction[#:eval the-eval
(new rocket% 7 -1)]

Both components can be accessed through the accessor methods:

@interaction[#:eval the-eval
(send (new rocket% 7 -1) height)
(send (new rocket% 7 -1) velocity)]

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
(check-expect (send (new rocket% 0 1) on-tick)
              (new rocket% 1 1))
(check-expect (send (new rocket% 10 -2) on-tick)
              (new rocket% 8 -2))
(check-expect (send (new rocket% 0 -1) on-tick)
              (new rocket% -1 1))
]

Based on this specification, we revise the @racket[on-tick] method
definition as follows:

@racketblock[
(define-class rocket%
  ...
  ;; on-tick : -> World 
  (define (on-tick)
    (new rocket%
         (+ (field velocity) (field height))
         (cond [(<= (field height) 0) (abs (field velocity))]
               [else (field velocity)])))
  ...)]

Giving us an overall program of:

@#reader scribble/comment-reader
(racketmod
class/0
(require 2htdp/image)
(require class/universe)

(define ROCKET (bitmap class/0/rocket.png))

(define WIDTH 100)
(define HEIGHT 300)
(define MT-SCENE (empty-scene WIDTH HEIGHT))

;; A World is a (new rocket% Number Number).
;; Interp: height represents the height of the rocket in AU,
;; velocity represents the speed of the rocket in AU/seconds.
(define-class rocket%
  (fields height velocity)
  
  ;; on-tick : -> World 
  (define (on-tick)
    (new rocket%
         (+ (field velocity) (field height))
         (cond [(<= (field height) 0) (abs (field velocity))]
               [else (field velocity)])))
  
  ;; to-draw : -> Scene
  (define (to-draw)
    (place-image ROCKET
                 (/ WIDTH 2) 
                 (- HEIGHT (field height))
                 (empty-scene WIDTH HEIGHT))))

(check-expect (send (new rocket% 0 1) to-draw)
              (place-image ROCKET 
                           (/ WIDTH 2) 
                           HEIGHT 
                           MT-SCENE))

(check-expect (send (new rocket% 0 1) on-tick)
              (new rocket% 1 1))
(check-expect (send (new rocket% 10 -2) on-tick)
              (new rocket% 8 -2))
(check-expect (send (new rocket% 0 -1) on-tick)
              (new rocket% -1 1))

(big-bang (new rocket% HEIGHT -1)))

@section{Adding a moon}

Adding more components is straightforward.

Here is a complete program that includes an orbiting moon:

@#reader scribble/comment-reader
(racketmod
class/0
(require 2htdp/image)
(require class/universe)

(define ROCKET (bitmap class/0/rocket.png))
(define MOON (circle 20 "solid" "blue"))

(define WIDTH 300)
(define HEIGHT 300)
(define MT-SCENE (empty-scene WIDTH HEIGHT))

; A World is a (new rocket% Number Number Number).
; Interp: height represents distance from the ground in AU.
; velocity represents the speed of the rocket in AU/sec.
; moon-height represents the height of the moon.
(define-class rocket%
  (fields height velocity moon-height)
  
  ;; on-tick : -> World 
  (check-expect (send (new rocket% 0 1 100) on-tick)
		(new rocket% 1 1 105))
  (define (on-tick)
    (new rocket%
         (+ (field velocity) (field height))
         (cond [(= (field height) 0) (abs (field velocity))]
               [else (field velocity)])
         (modulo (+ 5 (field moon-height)) 200)))
  
  ;; to-draw : -> Scene
  (check-expect (send (new rocket% 0 1 100) to-draw)
		(place-image MOON
			     (/ WIDTH 3) 
			     100
			     (place-image ROCKET 
					  (/ WIDTH 2) 
					  HEIGHT 
					  MT-SCENE)))
  (define (to-draw)
    (place-image MOON
                 (/ WIDTH 3) 
                 (field moon-height)
                 (place-image ROCKET
                              (/ WIDTH 2) 
                              (- HEIGHT (field height))
                              MT-SCENE))))

(big-bang (new rocket% HEIGHT -1 100))
)
 


