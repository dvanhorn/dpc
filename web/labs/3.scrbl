#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt"
          (for-label (except-in class1 define-struct)
                     2htdp/image
                     (only-in lang/htdp-intermediate-lambda define-struct)
                     class1/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class1))
    (the-eval '(require 2htdp/image))
    the-eval))

@(define exercise (exercise-counter))

@title[#:tag "lab03"]{1/24: Interfaces and inheritance}

@lab:section{Update your class system}

Throughout the course the @racket[class] languages will be under constant
development, so it's important that you keep up to date. In fact, we've already
updated it since the last lab.

@exercise{
  Update your class system. In DrRacket, go to @tt{File → Install .plt File... →
  Web} and copy in this URL:

  @indented{
    @tt[class-system-latest]
  }
}

The URL above will always point to the latest version of the class system. Since
DrRacket remembers the last URL you used in the @tt{Install .plt File...}
dialog, you should usually only have to open the dialog and click @tt{Ok} to
update to the latest version.

Information about the latest version is always available at @secref["class"].

@lab:section{Invaders}

A @racket[World] consists of @racket[Invaders], each of which has a location
(@racket[x], @racket[y]), can @racket[draw] itself, and can @racket[step] itself
through an animation. an @racket[Invader] is either a @racket[Ball] or a
@racket[Block].

@(racketmod class1) @; Report bug: racketmod loses one level of indentation
@#reader scribble/comment-reader
(racketblock
(require 2htdp/image)
(require class1/universe)

(define WIDTH  500)
(define HEIGHT 500)

; A World is a (new world% [Listof Invader])
(define-class world%
  (fields invaders)

  ; -> World
  ; Advance the World
  (define/public (on-tick)
    (new world% (map (λ (c) (send c step)) (field invaders))))

  ; -> Image
  ; Draw the World
  (define/public (to-draw)
    (foldr (λ (c scn) (place-image (send c draw)
                                   (send c x)
                                   (send c y)
                                   scn))
           (empty-scene WIDTH HEIGHT)
           (field invaders))))

; An Invader is one of:
;  - Ball
;  - Block
)

Here's a trick for working with 2D geometry: use
@link["http://docs.racket-lang.org/reference/generic-numbers.html?q=complex+numbers#(part._.Complex_.Numbers)"]{complex
numbers} @math{x+yi} to represent @math{(x,y)} locations. This is useful because
most geometric transformations can be done simply by adding, subtracting, or
multiplying complex numbers.

For instance, if you're at location @math{(2,5)} and want to translate 1 step
left and 2 steps down to get to location @math{(1,7)}, then you can use complex
addition to calculate @math{2+5i + -1+2i = 1+7i}.

@#reader scribble/comment-reader
(racketblock
; A Location is a Complex where (x,y) is represented as the complex number x+yi
; A Non-Negative is a non-negative Number
; A Color is a String

; A Ball is a (new ball% Location Non-Negative Color)
(define-class ball%
  (fields radius color location)

  ; -> Number
  ; The x-coordinate of the Ball
  (define/public (x)
    (real-part (field location)))

  ; -> Number
  ; The y-coordinate of the Ball
  (define/public (y)
    (imag-part (field location)))

  ; -> Image
  ; The image representing the Ball
  (define/public (draw)
    (circle (field radius) "solid" (field color)))

  ; -> Ball
  ; The next Ball in the animation sequence
  (define/public (step)
    (new ball%
         (field radius)
         (field color)
         (+ 0+1i (field location)))))

; A Block is a (new block% Location Non-Negative Non-Negative Color)
(define-class block%
  (fields width height color location)

  ; -> Number
  ; The x-coordinate of the Block
  (define/public (x)
    (real-part (field location)))

  ; -> Number
  ; The y-coordinate of the Block
  (define/public (y)
    (imag-part (field location)))

  ; -> Image
  ; The image representing the Block
  (define/public (draw)
    (rectangle (field width) (field height) "solid" (field color)))

  ; -> Block
  ; The next Block in the animation sequence
  (define/public (step)
    (new block%
         (field width)
         (field height)
         (field color)
         (+ 0+1i (field location)))))

  (big-bang (new world% (list (new ball%   5    "red"   50+10i)
                              (new ball%  10    "red"  150+10i)
                              (new ball%  20    "red"  250+10i)
                              (new ball%  10    "red"  350+10i)
                              (new ball%   5    "red"  450+10i)
                              (new block% 30 20 "blue"  50+60i)
                              (new block% 15 10 "blue" 150+60i)
                              (new block%  5  5 "blue" 250+60i)
                              (new block% 15 10 "blue" 350+60i)
                              (new block% 30 20 "blue" 450+60i))))
)

When you run this, you should see balls and blocks falling. (Simple
beginnings...)

As we said above, an Invader is something that has a @racket[location] along
with convenience methods for its @racket[x] and @racket[y] coordinates, can
@racket[draw] itself, and can @racket[step] itself through an animation.

@exercise{
  Define an @racket[invader<%>] interface for @racket[ball%] and @racket[block%]
  to implement.
}

The convenience methods @racket[x] and @racket[y] are useful, for instance, in
the World's @racket[to-draw] method where it needs to use the x- and
y-coordinates separately. But notice that the methods are implemented in the
same way for Balls and Blocks. Also notice that both classes have a
@racket[location] field. Moreover, whatever kinds of Invaders we introduce
later, we will probably want them to also have a @racket[location], an
@racket[x], and a @racket[y].

@exercise{
  Using inheritance, abstract the @racket[location] field and the @racket[x] and
  @racket[y] methods into a common superclass, @racket[invader%].
}

Also, the two @racket[step] methods for Balls and Blocks do essentially the same
thing---move the Invader down by 1---but they aren't expressed in a way that we
can abstract.

@exercise{
  Simplify each @racket[step] method: define a @racket[move] method that takes a
  location @racket[dz] and updates the @racket[location] by translating by
  @racket[dz]. (Just as @math{x} and @math{y} are commonly used for real
  numbers, @math{z} is commonly used for complex numbers.)

  Abstract @racket[step] into the superclass.
}

@lab:section{Invasion}

@exercise{
  End the game when any Invader successfully invades. Use an @racket[invaded?]
  method on Invaders to test whether the Invader has reached the bottom of the
  screen, and end the game when any has done so.
}

When an Invader reaches the bottom of the screen, you lose---but for this to be
any fun, we need a way to win! Next we will add a spaceship to the bottom of the
screen to defend against the invaders:

@elem[@image["labs/3/spaceship.png"] #:style "center"]

The ship won't do much: it just updates its position to the horizontal position
of the mouse. It's vertical position stays fixed.

@exercise{
  Add a Ship to the game. Fix its vertical position near the bottom of the
  screen and keep its horizontal position aligned with the position of the
  mouse.

  Suggestion: Define a @racket[ship%] class that understands @racket[location],
  @racket[x], @racket[y], and @racket[draw]. (They can be either methods or
  fields.)
}

How much did you have to change your @racket[to-draw] method in @racket[world%]?
I hope not much...

A Ship isn't an Invader since it doesn't @racket[step] and never needs to answer
@racket[invaded?], but it does @racket[draw] and have an @racket[x], @racket[y],
and @racket[location]. Moreover, @racket[to-draw] in @racket[world%] only relies
on these last four behaviors---but we lack an interface to codify it.

@exercise{
  Split the @racket[invader<%>] interface into two: a @racket[drawable<%>]
  interface to support the needs of @racket[to-draw], and a new
  @racket[invader<%>] interface that just includes @racket[step] and
  @racket[invaded?].

  Of your classes, which should implement which interfaces?---note that a class
  can implement multiple interfaces.
}

Now compare your @racket[ship%] class with your @racket[invader%] class:
@racket[invader%] has a @racket[location] field with @racket[x] and @racket[y]
methods reading from it, and the @racket[ship%] class you just defined should
have something very similar.

@exercise{
  Abstract the @racket[location] field and the @racket[x] and @racket[y] methods
  out of @racket[ship%] and @racket[invader%] and into a common superclass,
  @racket[drawable%].
}

@exercise{
  Now you have a variety of interfaces, classes, and inheritance relationships.
  Take a moment to sanity check each of them. Which classes should implement
  which interfaces? Which superclasses exist only to be inherited from?
}

@lab:section{Surviving invasion}

To survive the invasion, the Ship must shoot some kind of projectile at the
Invaders: bullets, lasers, bananas---your choice.

@exercise{
  Define a class for your projectile. For the sake of concreteness, I'll assume
  you chose Banana. Bananas must (1) have a location, (2) know how to draw
  themselves, and (3) step upward over time.

  Which of the superclasses can you inherit from to save yourself work? Which
  aren't appropriate to inherit from?

  Which interfaces do Bananas implement?
}

@exercise{
  Add a list of Bananas to the World. Draw them when the World draws, and step
  them when the World ticks.
}

But whence Bananas?

@exercise{
  Add a @racket[shoot] method to @racket[ship%] that creates a new Banana at the
  Ship's location. Add a @racket[shoot] method to @racket[world%] that asks the
  Ship to shoot and begins tracking its newly fired Banana.

  Fire Bananas when the user clicks the mouse.
}

Too many Bananas!

@exercise{
  Remove Bananas after they leave the screen. Add an @racket[on-screen?] method
  to @racket[banana%], and remove off-screen Bananas on each World tick.
}

If a Banana falls in a forest...

@exercise{
  Add a @racket[contains?] method to the @racket[invader<%>] interface that
  takes a Location and computes whether the location is within the spatial
  extent of the Invader.

  Implement @racket[contains?] appropriately for each of @racket[ball%] and
  @racket[block%]. Note that circles and rectangles occupy different parts of
  space.
}

@exercise{
  Add a @racket[zapped?] method to @racket[invader<%>] and @racket[invader%]
  that takes a list of Bananas as input and tests whether the Invader has been
  hit by any of them. Each tick, remove all Invaders from the World that are
  being zapped by a Banana.

  For the purposes of detecting a collision, just test to see if the center of
  the Banana is contained in the Invader. (For added realism, try drawing your
  Bananas as very small dots. Or if you really want to try proper shape
  intersection, try something shaped not like a banana.)
}

@lab:section{Go, banana}

@exercise{@bold{(Open ended)}
  Now that we've laid the groundwork for our new hit iPhone game, all that's
  left is a few splashes of creativity. Below are a few ideas---and remember: a
  little randomness can substitute for a lot of complexity.
  @itemlist[
    @item{Add new shapes of Invaders}
    @item{Give Invaders more complicated movement}
    @item{Let Invaders shoot back}
    @item{Increase the difficulty by using keyboard instead of mouse control}
    @item{Spawn new Invaders over time}
  ]
}
