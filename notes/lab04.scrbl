#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          "../web/lab.rkt"
          "../web/unnumbered.rkt"
          "../web/utils.rkt"
          (for-label (except-in class1 define-struct)
                     2htdp/image
                     (only-in lang/htdp-intermediate-lambda define-struct)
                     class1/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class1))
    (the-eval '(require 2htdp/image))
    ;(the-eval '(require class1/universe))
    the-eval))

@(define exercise (exercise-counter))

@title[#:tag "lab04"]{Universe}

@lab:section{Universe with objects}

@#reader scribble/comment-reader
(racketmod
class1
(require class1/universe)
(require 2htdp/image)

(define WIDTH  500)
(define HEIGHT 500)
)

@exercise{
  Create a World that connects to the Universe using the @racket[register]
  method. The IP of the Universe machine is written on the board.

  You must include a @racket[to-draw] method---for now just have it create an
  empty scene with the dimensions above.

  If you don't include an @racket[on-receive] method then the Universe will
  disconnect you immediately, after it tries to send you a message and you fail
  to receive it. To get started, just create one that returns the World
  unchanged.
}

The Universe consists of circular rocks flying through space. You are one of
these rocks, and if you collide with another you will be destroyed and the
Universe will disconnect you.

The Universe keeps track of all the rocks. Your job is to build a World to give
you a visual display of what's going on in the Universe and give you control
over your rock so that you can navigate it away from danger.

The Universe communicates to you by sending you the list of all the rocks every
@racket[1/10]th of a second. Each rock is sent over the wire as RockData:

@#reader scribble/comment-reader
(racketblock
; A  RockData is a (list Location Velocity Acceleration Radius Color)

; A  Location is a Complex
; A  Velocity is a Complex
; An Acceleration is a Complex
; A  Radius is a non-negative Real
; A  Color is a String
)

We will again use complex numbers for 2D geometry like we did in
@seclink["lab03"]{lab 3}.

@exercise{
  Define a Rock class. It should have fields for all the data in RockData, and
  it should know how to @racket[draw] itself.
}

The Universe sends you RockData by calling your @racket[on-receive] method. The
input SExp is a list of RockData.

@exercise{
  Elaborate @racket[on-recieve] and @racket[to-draw] to display the current
  state of all the Rocks in the Universe.
}

You can only control yourself by sending messages back to the Universe. In fact,
all you can control is your acceleration. The
@link["http://docs.racket-lang.org/teachpack/2htdpuniverse.html#(part._.Sending_.Messages)"]{Package}
you may send to the Universe consists of one value: your new Acceleration.

@exercise{
  Add simple keyboard controls to your World so that you can control your
  acceleration.
}

Notice that your acceleration stays constant until you send a new one...

@exercise{@bold{(Open ended)}
  Using either the mouse or the keyboard, design a control scheme that gives you
  precise control over your Rock. Consider how you can both (1) control the
  angle of your acceleration, and (2) modulate the magnitude of your
  acceleration.
}

@exercise{@bold{(Open ended)}
  The Universe gives you the location (and velocity) of every Rock in play, and
  yours is always the first in the list. Design an @emph{automated} control
  scheme that avoids other Rocks.

  Here's a basic approach to get you started: assign a repulsive acceleration to
  each rock, weighted by its distance from you, and sum them all together to
  find the direction of least ``immediate'' danger. (This strategy will need a
  bit of refinement...)
}
