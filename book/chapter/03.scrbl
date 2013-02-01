#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda check-expect define-struct ...))
          (for-label (except-in class/0 define-struct ... length check-expect))
          (for-label 2htdp/image)
          (for-label class/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class/0))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    (the-eval '(require book/chapter/03/light))
    the-eval))

@title[#:tag "Interfaces"]{Classes of Objects: Interface Definitions}

In this chapter, we take an alternative perspective on defining sets
of objects; we can characterize objects not just by their
construction, as done with a data definition, but also by the methods
they support.  We call this characterization an interface definition.
As we'll see, designing to interfaces leads to generic and extensible
programs.

@section{Lights, revisited}

Let's take another look at the @tt{Light} data definition we developed
in @secref{Enumerations}.  We came up with the following data definition:

@classblock{
;; A Light is one of:
;; - (new red%)
;; - (new green%)
;; - (new yellow%)
}

We started with a @racket[next] method that computes the successor for
each light.  Let's also add a @racket[draw] method and then build a
@racket[big-bang] animation for a traffic light.

@#reader scribble/comment-reader
(racketmod
  class/0
  (require 2htdp/image)
  (define LIGHT-RADIUS 20)

  (define-class red%
    ;; next : -> Light
    ;; Next light after red
    (check-expect (send (new red%) next) (new green%))
    (define (next)
      (new green%))

    ;; draw : -> Image
    ;; Draw this red light
    (check-expect (send (new red%) draw)
                  (circle LIGHT-RADIUS "solid" "red"))
    (define (draw)
      (circle LIGHT-RADIUS "solid" "red")))

  (define-class green%
    ;; next : -> Light
    ;; Next light after green
    (check-expect (send (new green%) next) (new yellow%))
    (define (next)
      (new yellow%))

    ;; draw : -> Image
    ;; Draw this green light
    (check-expect (send (new green%) draw)
                  (circle LIGHT-RADIUS "solid" "green"))
    (define (draw)
      (circle LIGHT-RADIUS "solid" "green")))

  (define-class yellow%
    ;; next : -> Light
    ;; Next light after yellow
    (check-expect (send (new yellow%) next) (new red%))
    (define (next)
      (new red%))

    ;; draw : -> Image
    ;; Draw this yellow light
    (check-expect (send (new yellow%) draw)
                  (circle LIGHT-RADIUS "solid" "yellow"))
    (define (draw)
      (circle LIGHT-RADIUS "solid" "yellow"))))

We can now create and view lights:

@interaction[#:eval the-eval
(send (new green%) draw)
(send (new yellow%) draw)
(send (new red%) draw)
]

To create an animation we can make the following world:

@classblock{
(define-class world%
  (fields light)
  (define (tick-rate) 5)
  (define (to-draw)
    (send (send this light) draw))
  (define (on-tick)
    (new world% (send (send this light) next))))

(require class/universe)
(big-bang (new world% (new red%)))
}

At this point, let's take a step back and ask the question: @emph{what
is essential to being a light?}  Our data definition gives us one
perspective, which is that for a value to be a light, that value must
have been constructed with either @racket[(new red%)], @racket[(new
yellow%)], or @racket[(new green%)].  But from the world's
perspective, what matters is not how lights are constructed, but
rather what can lights compute.  All the world does is call methods on
the light it contains, namely the @racket[next] and @racket[draw]
methods.  We can rest assured that the light object understands the
@racket[next] and @racket[draw] messages because, by definition, a
light must be one of @racket[(new red%)], @racket[(new yellow%)], or
@racket[(new green%)], and each of these classes defines @racket[next]
and @racket[draw] methods.  But it's possible we could relax the
definition of what it means to be a light by just saying what methods
an object must implement in order to be considered a light.  We can
thus take a constructor-agnostic view of objects by defining a set of
objects in terms of the methods they understand.  We call a set of
method signatures (i.e., name, contract, and purpose statement) an
@emph{interface}.

@section{A light of a different color}

So let's consider an alternative characterization of lights not in
terms of @emph{what they are}, but rather @emph{what they do}.
Well a light does two things: it can render as an image and it can 
transition to the next light; hence our @emph{interface definition}
for a light is:

@classblock{
;; An ILight implements
;; next : -> ILight
;; Next light after this light.
;; draw : -> Image
;; Draw this light.
}

Now it's clear that every @tt{Light} is an @tt{ILight} because every
@tt{Light} implements the methods in the @tt{ILight} interface, but we
can imagine new kinds of implementations of the @tt{ILight} interface
that are not @tt{Light}s.  For example, here's a class that implements
the @tt{ILight} interface:

@classblock{
;; A ModLight is a (new mod-light% Natural)
;; Interp: 0 = green, 1 = yellow, otherwise red.
(define-class mod-light%
  (fields n)
  ;; next : -> ILight
  ;; Next light after this light.
  (define (next)
    (new mod-light% (modulo (add1 (send this n)) 3)))

  ;; draw : -> Image
  ;; Draw this light.
  (define (draw)
    (cond [(= (send this n) 0)
           (circle LIGHT-RADIUS "solid" "green")]
          [(= (send this n) 1)
           (circle LIGHT-RADIUS "solid" "yellow")]
          [else
           (circle LIGHT-RADIUS "solid" "red")])))
}

Now clearly a @tt{ModLight} is never a @tt{Light}, but every
@tt{ModLight} is an @tt{ILight}.  Moreover, any program that is
written for @tt{ILight}s will work @emph{no matter what implementation
we use}.  So notice that the world program only assumes that its
@tt{light} field is an @tt{ILight}; this is easy to inspect---the
world never assumes the light is constructed in a particular way, it
just calls @racket[next] and @racket[draw].  Which means that if we were
to start our program off with 

@classblock{
(big-bang (new world% (new mod-light% 2)))
}

it would work exactly as before.

@section{Representation inpedendence and extensibility}

We've now developed a new concept, that of an @emph{interface}, which
is a collection of method signatures.  We say that an object @emph{is}
an instance of an interface whenever it implements the methods of the
interface.

The idea of an interface is already hinted at in the concept of a
union of objects since a function over a union of data is naturally
written as a method in each class variant of the union.  In other
words, to be an element of the union, an object must implement all the
methods defined for the union---the object must implement the union's
interface.  But interfaces are about more than just unions.  By
focusing on interfaces, we can see there are two important engineering
principles that can be distilled even from this small program:

@itemlist[#:style 'ordered
  @item{Representation independence

As we've seen with the simple world program that contains a light,
when a program is written to use only the methods specified in an
interface, then the program is @emph{representation independent} with
respect to the interface; we can swap out any implementation of the
interface without changing the behavior of the program.}

  @item{Extensibility

When we write interface-oriented programs, it's easy to see that they
are @emph{extensible} since we can always design new implementations
of an interface.  Compare this to the construction-oriented view of
programs, which defines a set of values once and for all.}]

These points become increasingly important as we design larger and
larger programs.  Real programs consist of multiple interacting
components, often written by different people.  Representation
independence allows us to exchange and refine components with some
confidence that the whole system will still work after the change.
Extensibility allows us to add functionality to existing programs
without having to change the code that's already been written; that's
good since in a larger project, it may not even be possible to edit a
component written by somebody else.

Let's look at the extensiblity point in more detail.  Imagine we had
developed the @tt{Light} data definition and its functionality along
the lines of @emph{HtDP}.  We would have (we omit @racket[draw] for
now):

@classblock{
;; A Light is one of:
;; - 'Red
;; - 'Green
;; - 'Yellow

;; next : Light -> Light
;; Next light after the given light
(check-expect (next 'Green) 'Yellow)
(check-expect (next 'Red) 'Green)
(check-expect (next 'Yellow) 'Red)
(define (next l)
  (cond [(symbol=? 'Red l) 'Green]
        [(symbol=? 'Green l) 'Yellow]
        [(symbol=? 'Yellow l) 'Red]))
}

Now imagine if we wanted to add a new kind of light---perhaps to
represent a blinking yellow light.  For such lights, let's assume
the next light is just a blinking yellow light:

@classblock{
(check-expect (next 'BlinkingYellow) 'BlinkingYellow)
}

That's no big deal to implement @emph{if we're allowed to revise
@racket[next]}---we just add another clause to @racket[next] handle
@racket['BlinkingYellow] lights.  But what if we can't?  What if
@racket[next] were part of a module provided as a library?  Well then
life is more complicated; we'd have to write a new function, say
@racket[fancy-next], that handled blinking lights and used
@racket[next] for all non-blinking lights.  And while that gets us a
new function with the desired behavior, that won't do anything for all
the places the @racket[next] function is used.  If we're able to edit
the code that uses @racket[next], then we can replace each use of
@racket[next] with @racket[fancy-next], but what if we can't...?  Well
then we're just stuck.  If we cannot change the definition of
@racket[next] or all the places it is used, then it is not possible to
extend the behavior of @racket[next].

Now let's compare this situation to one in which the original program
was developed with objects and interfaces.  In this situation we have
an interface for lights and several classes, namely @racket[red%],
@racket[yellow%], and @racket[green%] that implement the @racket[next]
method.  Now what's involved if we want to add a variant of lights
that represents a blinking yellow light?  We just need to write a
class that implements @racket[next]:

@classblock{
;; Interp: blinking yellow light
(define-class blinking-yellow%
  ;; next : -> ILight
  ;; Next light after this blinking yellow light.
  (check-expect (send (new blinking-yellow%) next)
                (new blinking-yellow%))
  (define (next) this))
}

Notice how we didn't need to edit @racket[red%], @racket[yellow%], or
@racket[green%] at all!  So if those things are set in stone, that's
no problem.  Likewise, programs that were written to use the light
interface will now work even for blinking lights.  We don't need to
edit any uses of the @racket[next] method in order to make it work for
blinking lights.  This program is truly extensible.

@section{Sharing Interfaces}

@classblock{
;; A Posn implements
;; move-by : Real Real -> Posn
;; move-to : Real Real -> Posn
;; dist-to : Posn -> Real

;; A Segment implements
;; draw-on : Scene -> Scene
;; move-by : Real Real -> Segment
;; move-to : Real Real -> Segment
;; dist-to : Posn -> Real
}

Notice that any object that is a @tt{Segment} is also a @tt{Posn}.

@section{Exercises}

@subsection{Beings, Zombies, and You}

Q: I've designed a single interface @tt{Being} that subsumes both
@tt{Zombie} and @tt{Player} in the current assignment.  Do I still
have to design a @tt{Zombie} and @tt{Player} interface?

A: Yes.  There are a couple reasons for this.  One is that there
really are some differences between the operations that should be
supported by a player versus a zombie.  For example, zombies eat
brains; players don't.  Another is that, as you are probably noticing,
much of this course is about interface specification and
implementation.  As we build larger and larger programs, interfaces
become a much more important engineering tool.  An interface can be
viewed as a contract---an agreement on the terms of
engagement---between the @emph{implementor} and the @emph{consumer} of
a software component.  In this assignment, even though you are acting
simultaneously as both of these parties, we are asking you to write
down the agreement you are making between the world program that uses
zombies and players and the classes that implement zombies and
players.  Part of our agreement with you, is that you'll write
separate specifications; so that's what you need to do.

That said, if really believe that there should be a single uniform
interface that all zombies @emph{and} players should adhere to, you
can write a @tt{Being interface and program to it}.

Revise your Zombie! program.

@subsection{Super Zombie!}

Revise your design of the Zombie game to include a @tt{Zombie}
and @tt{Player} interface.  Implement a @racket[live-zombie%]
and @racket[dead-zombie%] class that both implement your
@tt{Zombie} interface; implement a @racket[player%] class that
implements your @tt{Player} interface.

@; Do not use the functional abstraction recipe.  Instead, if you notice
@; code that could be shared between the various classes you've
@; designed, design super classes and use inheritance to abstract the
@; duplicated code.

 Design a @racket[world%] class for playing the Zombie game that
 interacts with the zombie and player objects only according to the
 interfaces you've designed, i.e. the @racket[world%] class should
 work for @emph{any} objects that correctly implement your zombie and 
 player interfaces.

@subsection{Modulo Zombie!}

 Using your interface design from the previous problem, design a 
 @racket[modulo-player%] class and a @racket[modulo-live-zombie%]
 class that implement the @tt{Player} and @tt{Zombie}
 interfaces, respectively.

 These alternative implementations should behave as follows: the
 player and the zombies may now ``wrap around'' on the screen.  If a
 player goes off the top of the screen, they should re-appear on the
 bottom; if the go off of the left side, they should appear on the
 right, etc., and likewise for the zombies.  When calculating in which
 direction they should go, the player and zombies should take into
 account the possibility of wrapping around the screen.  So for
 example, if the player is on the far right side and there is a zombie
 on the far left side, the zombie should head left to wrap around the
 screen and quickly arrive upon the player and feast upon his or her
 brains.  Similarly if the mouse is on the very top and the player is
 on the very bottom, the player should move down to get to the top
 quickly.

 If you need to make changes to your interface design to accommodate
 these new game requirements, you must re-implement your solution to
 problem 2 in order to satisfy the revised interfaces.  In the end,
 the interfaces used and your implementation of the @racket[world%]
 class be the same in both problem 2 and 3.

 @subsection{Mixed Zombie!}

 Experiment with different combinations of your classes from the
 previous exercises (only the player can wrap around; only the zombies
 can wrap around; some of the zombies and the player; some of the
 zombies, but not the player, etc.) until you find a combination you
 like best.  Write down an expression that launches the game using
 this combination.




