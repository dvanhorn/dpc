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

@internal{

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

@lab:section{Creatures}

Let's pick up some of the pieces from last lab. A @racket[World] consists of
@racket[Creatures], each of which has a location (@racket[x], @racket[y]), can
@racket[draw] itself, and can @racket[step] itself through an animation. A
@racket[Creature] is either a @racket[Ball] or a @racket[Block].

@(racketmod class1) @; Report bug: racketmod loses one level of indentation
@#reader scribble/comment-reader
(racketblock
(require 2htdp/image)
(require class1/universe)

; A World is a (new world% [Listof Creature])
(define-class world%
  (fields creatures)

  ; -> World
  ; Advance the World
  (define/public (on-tick)
    (new world% (map (λ (c) (send c step)) (field creatures))))

  ; -> Image
  ; Draw the World
  (define/public (to-draw)
    (foldr (λ (c scn) (place-image (send c draw)
                                   (send c x)
                                   (send c y)
                                   scn))
           (empty-scene 500 500)
           (field creatures))))

; A Creature is one of:
;  - Ball
;  - Block
)

Here's a trick for working with 2D geometry: use complex numbers @math{x+yi} to
represent @math{(x,y)} locations. This is useful because most geometric
transformations can be done simply by adding, subtracting, or multiplying
complex numbers.

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
  (fields location radius color)

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
         (+ 0+1i (field location))
         (field radius)
         (field color))))

; A Block is a (new block% Location Non-Negative Non-Negative Color)
(define-class block%
  (fields location width height color)

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
         (+ 0+1i (field location))
         (field width)
         (field height)
         (field color))))

(big-bang (new world% (list (new ball% 150+50i 10 "red")
                            (new block% 350+50i 10 20 "blue"))))
)

When you run this, you should see a ball and block falling. (Simple
beginnings...)

As we said above, a Creature is something that has a @racket[location] along
with convenience methods for its @racket[x] and @racket[y] coordinates, can
@racket[draw] itself, and can @racket[step] itself through an animation.

@exercise{
  Define a @racket[creature<%>] interface for @racket[ball%] and @racket[block%]
  to implement.
}

The convenience methods @racket[x] and @racket[y] are useful, for instance, in
the World's @racket[to-draw] method where it needs to use the x- and
y-coordinates separately. But notice that the methods are implemented in the
same way for Balls and Blocks. Also notice that both classes have a
@racket[location] field---and whatever kinds of Creatures we introduce later, we
will probably want them to also have a location.

@exercise{
  Using inheritance, abstract the @racket[location] field and the @racket[x] and
  @racket[y] methods into a common superclass, @racket[creature%].
}

Also, the two @racket[step] methods for Balls and Blocks do essentially the same
thing---move the Creature down by 1---but they aren't expressed in a way that we
can abstract.

@exercise{
  Simplify each @racket[step] method: define a @racket[move] method that takes a
  location @racket[z] and updates the @racket[location] by translating by
  @racket[z]. (Just as @math{x} and @math{y} are commonly used for real numbers,
  @math{z} is commonly used for complex numbers.)

  Abstract @racket[move] into the superclass.
}

@lab:section{Collision detection}

@exercise{
  Add a method to Creatures, @racket[contains], that takes ...
}

}
