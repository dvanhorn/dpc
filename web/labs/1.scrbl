#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          "../unnumbered.rkt"
          "../utils.rkt"
          (for-label (except-in class0 define-struct)
                     2htdp/image
                     (only-in lang/htdp-intermediate-lambda define-struct)
                     class0/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class0))
    (the-eval '(require 2htdp/image))
    the-eval))

@title[#:tag "lab01"]{1/10: Classes and objects}

@internal{

@section{Subversion test run: Assignment 0}

@exercise{
  Find your @seclink["Initial_partnership_assignments"]{homework partner}
}

@exercise{
  Familiarize yourself with @seclink["Subversion"]{how we will use Subversion}
}

@exercise{
  Create and submit a dummy Assignment 0 with your homework partner:
  @itemlist[
    @item{Problem 1: Add some numbers together}
    @item{Problem 2: Create an image}
  ]
}

@section{Setting up @racket[class0]}

To write object-oriented programs we will use a special dialect of Racket called
@racket[class0] that includes support for classes and objects. Just like we
progressed through BSL, ISL, and ASL last semester, we will progress from
@racket[class0] on to richer object-oriented dialects as we learn more about how
to program with objects.

To install the @racket[class0] language in DrRacket, go to @tt{File → Install
.plt File... → Web} and copy in this URL:

@indented{
  @tt["http://www.ccs.neu.edu/course/cs2510h/class-system-lastest.plt"]
  @;@tt[(format "http://www.ccs.neu.edu/course/cs2510h/~a" plt-filename)]
}

Once installed, use the @racket[class0] language by selecting @tt{Language →
Choose Language... → Use the language declared in the source}. Then start your
file with the following lines:

@#reader scribble/comment-reader
(racketmod class0
  (require 2htdp/image)
  (require class0/universe)
)

The @tt{#lang} line tells Racket to use the @racket[class0] language, and the
two @racket[require] lines load the image and universe libraries that we'll use
to create images and interactive programs.

@section{Classes and objects}

Here is an example class in Racket to represent balls that we can ask geometric
questions of and draw to the screen.

@#reader scribble/comment-reader
(racketblock+eval #:eval the-eval
  ; A Ball is a (new ball% Number Number Number Color)
  (define-class ball%
    (fields x y radius color)

    ; -> Number
    ; The Ball's area
    (define/public (area)
      (* pi (sqr (field radius))))

    ; Ball -> Boolean
    ; Do the Balls have the same radius?
    (define/public (same-radius? b)
      (equal? (field radius)    ; (How to access our own field)
              (send b radius))) ; (How to access someone else's field)

    ; Ball -> Boolean
    ; Do the Balls have the same area?
    (define/public (same-area? b)
      (equal? (area)            ; (How to call our own method)
              (send b area)))   ; (How to call someone else's method)

    ; -> Image
    ; The image representing the Ball
    (define/public (draw)
      (circle (field radius) "solid" (field color))))
)

We can create and use Ball objects as follows:

@interaction[#:eval the-eval
  (define b (new ball% 50 25 10 "red"))
  b
  (send b x)
  (send b radius)
  (send b area)
  (send b same-radius? (new ball% 10 20 3 "red"))
  (send b same-area? (new ball% 10 20 3 "red"))
  (send b draw)
]

We can add new Ball behaviors by adding methods to the @racket[ball%] class.

@exercise{
  Write a @racket[circumference] method for the class @racket[ball%] that
  calculates the Ball's circumference.
}

@exercise{
  Write a @racket[distance-to] method for the class @racket[ball%] that takes
  another Ball and returns the distance between the centers of the two Balls.
}

@exercise{
  Write an @racket[overlaps?] method for the class @racket[ball%] that takes
  another Ball and determines whether the two Balls overlap.
}

Let's construct another class to represent rectangles.

@exercise{
  Write a @racket[block%] class to represent rectangular blocks on the screen.
  Include @racket[x] and @racket[y] fields to record its position,
  @racket[width] and @racket[height] fields to record its size, and a
  @racket[color] field. Construct some examples of Blocks.
}

@exercise{
  Write a @racket[diagonal] method for @racket[block%] that calculates the
  length of the rectangle's diagonal.
}

@exercise{
  Write a @racket[draw] method for @racket[block%] that returns an image
  representing the Block.
}

@section{The World as an object}

Recall how we used @racket[big-bang] last semester:

@#reader scribble/comment-reader
(racketblock
  ; A World is a (make-world ...)
  (define-struct world (...))

  ; tick : World -> World
  (define (tick w)
    ...)

  ; draw : World -> Image
  (define (draw w)
    ...)

  (big-bang (make-world ...)
            (on-tick tick)
            (on-draw draw))
)

We would define functions to implement the various behaviors of the World and
then pass each of them off to @racket[big-bang], along with an initial World
value. In other words, @racket[big-bang] needed a combination of
@emph{structure}---the initial World---and @emph{functions}---the various
behaviors. But we know that

@indented[@emph{
  structure + functions = object
}]

So here's how we will use our new version of @racket[big-bang] that expects just
a single object:

@#reader scribble/comment-reader
(racketblock
  ; A World is a (new world% ...)
  (define-class world%
    (fields ...)

    ; -> World
    (define/public (on-tick)
      ...)

    ; -> Image
    (define/public (to-draw)
      ...))

  (big-bang (new world% ...))
)

@racket[big-bang] is now a function that takes an object (here, @racket[(new
world% ...)]) which @emph{must} respond to the @racket[to-draw] method, and
@emph{may} respond to a handful of others, including @racket[on-tick],
@racket[on-mouse], and @racket[on-key]---see the @racket[big-bang] docs for the
full list. To create such an object, we define a class with the desired methods.
Here we use the name @racket[world%], but you can call it anything you like.

@exercise{
  Using @racket[big-bang], create an animation where the user clicks to place
  Balls on the screen. Each click adds a Ball to the screen at the location of
  the mouse. For now, all the Balls can be the same color and size.

  To use @racket[big-bang], you'll need to construct an object, and to construct
  an object, you'll need to write a class. Which fields and methods should you
  include?
}

Let's add some variety by randomly selecting various aspects of what we put on
the screen. Here are a few useful functions for choosing things randomly:

@#reader scribble/comment-reader
(racketblock
  ; random-between : Integer Integer -> Integer
  ; Randomly choose a number between a and b (inclusive)
  (define (random-between a b)
    (+ b (random (+ 1 (- b a)))))

  ; choose : [Listof X] -> X
  ; Randomly choose an element from the list
  (define (choose xs)
    (list-ref xs (random (length xs))))

  ; random-color : -> Color
  ; Randomly choose a color from a set of common colors
  (define (random-color)
    (choose (list "red" "blue" "green" "yellow" "orange" "purple" "black")))
)

@exercise{
  Extend your animation so that when the user creates a Ball, its size and color
  of the Ball is chosen randomly.
}

@exercise{
  Modify your animation to use Blocks instead of Balls. Which methods need to
  change?
}

@exercise{
  Extend your animation to use Blocks @emph{and} Balls---let's call them,
  collectively, Creatures:

  @#reader scribble/comment-reader
  (racketblock
    ; A Creature is one of:
    ;  - Ball
    ;  - Block
  )

  When the user clicks, first randomly choose whether to create a Block or a
  Ball, and then randomly choose its parameters.
}

So far our animation isn't very animate. Let's teach our Creatures how to change
over time.

@exercise{
  Add a @racket[step] method to both Balls and Blocks. Sending @racket[step] to
  a Ball should return a new Ball that has been moved or resized in some way,
  and sending @racket[step] to a Block should return a new Block that has been
  moved or resized in some other way.
}

@exercise{
  Add an @racket[on-tick] method to your class of Worlds that @racket[step]s all
  of its creatures.
}

Now we have a basic framework for animating various kinds of Creatures. Unless
you've done something strange, any objects that understand the @racket[x],
@racket[y], @racket[draw], and @racket[step] methods should slot in with minimal
changes.

@exercise{
  @bold{(Open ended)} Create new kinds of Creatures with new kinds of movement.
  Try some of these ideas:

  @itemlist[
    @item{speeding up over time}
    @item{changing direction over time}
    @item{moving randomly}
    @item{moving randomly with a bias}
    @item{gravitating toward a fixed point}
    @item{gravitating toward each other}
    @item{moving in circles}
    @item{moving in spirals}
  ]

  If large numbers of off-screen or invisible Creatures are slowing down your
  animation, detect when they disappear and remove them.
}

}
