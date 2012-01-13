#lang scribble/manual
@(require "../class/utils.rkt"
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/1 define-struct ... length))
	  (for-label 2htdp/image)
	  (for-label class/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class/1))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))


@title{Interfaces}

[This section is still under construction and will be posted soon.  Please
check back later.]

@section{Lights, redux}

@itemlist[
 @item{a look back at light}
 @item{add a draw method}
 @item{make it a world program that cycles through the lights}
 @item{what does it mean to be a light?  next and draw}
 @item{alternative design of light class}
 @item{Q: what changes in world?  A: nothing.}
 @item{add interface to light design.  both alternatives implement it,
  and the world will work with anything that implements it}]


Q: Is it possible to represent the empty list with @racket[empty]?

A: You won't be able to represent the empty list with a value that
 is not an object.  The problem asks you to implement @emph{methods}
 that can be invoked on lists; the requirement to have methods
 dictates that lists are represented as objects since only an object
 can understand method sends.  If the empty list were represented with
 @racket[empty], then @racket[(send empty length)] would blow-up when
 it should instead produce @racket[0].  Moreover, if the empty list
 were not represented as an object, the design of a @racket[cons%] class will
 break too since the template for a method in the @racket[cons%] class
 is something along the lines of:
 @racketblock[
 (define-class cons%
   (fields first rest)
   (define (method-template ...)
     (field first) ...
     (send (field rest) method-template ...)))
]

Notice that the method is sent to the rest of this list.  If the rest
of the list is empty, then this will break unless the empty list is an
object, and moreover, an object that understands @racket[method-template].


Q: The @racket[define-interface] mechanism is a way of enforcing
that a class implements a certain set of method names, but is there a
way of enforcing things like the number of arguments one of these
methods must accept?

A: There are no linguistic mechanisms, but there are
@emph{meta}-linguistic mechanisms.  This is no different than our
approach last semester to enforcing things like the number of
arguments a function accepts.  Our approach has been to write down
contracts and purpose statements, which specify these things, and unit
tests, which check that we've followed the specification.  Our
language does not enforce these specifications, but that is really
just a detail of the particulars of the languages we've been using.
Even though contracts are not a language feature (in the languages
we've used so far), they are an important concept for organizing
software components.

It is important to keep in mind that an interface is more than a set
of method names.  Think of this analogously to structures:
@racket[define-struct] gives you a mechanism for defining structures,
but a structure does not a data definition make.  We now have
@racket[define-interface], but an interface is more than a set of
method names; it is a set of method names with contracts and purpose
statements.  Only the method name part is enforced, but the concept of
an interface is what's important.  It is a useful tool for organizing
and developing software components.  @emph{Even if we didn't have
@racket[define-interface], interfaces would be a useful conceptual
tool for writing programs}.

@section{Exercises}

 @subsection{Parametric lists}

 Consider the parametric data definition for lists we studied last
 semester:

 @#reader scribble/comment-reader
(racketblock 
 ;; A [Listof X] is one of:
 ;; - empty
 ;; - (cons X [Listof X])
)

Design an analogous class-based representation of parametric lists.
Design a @racket[list<%>] interface that includes @racket[cons],
@racket[empty], @racket[length], @racket[append], @racket[reverse],
@racket[map], @racket[filter], @racket[foldl], and @racket[foldr].

Implement that interface in two ways:
@itemlist[

 @item{Using the recipe for a recursive union represented using
objects, i.e.  similar to the way you developed lists of numbers last
week.}

 @item{Using a "wrapper class", i.e. design a class that has a single
field which contains a "real" list---one built out of @racket[cons]
and @racket[empty].}

]

Any program that interacts with either of these representations
according to the interface should not be able to tell them apart.

Use inheritance to lift method definitions to a super class to the
full extent possible.  (@emph{Hint}: it will help if you realize that many of
these methods may be expressed in terms of a few "core" methods.)  If
possible, have both the recursive union representation and the wrapper
representation share a common super class.

The @racket[cons] and @racket[empty] methods have been added to
facilitate opportunities for abstraction.  You might find them useful
to use when you lift methods to a common super class so that the right
kind of list (either a wrapped or a recursive union list) is
constructed.

Another hint: the names of methods we have chosen overlap with the
name of some standard values that you may like to use when defining
methods, especially in the wrapped list case.  If you refer to these
names within a class, you refer to the method rather than the built-in
value.  If you would like to refer to the built-in value, an easy
work-around is to do something like this:

@#reader scribble/comment-reader
(racketblock
  (define ls:cons cons) ; etc.
)

Now when you want to refer to the @racket[cons] @emph{function} instead
of the @racket[cons] @emph{method}, you can use the name @racket[ls:cons].

@subsection{Super Zombie!}

Revise your design of the Zombie game to include a @racket[zombie<%>]
and @racket[player<%>] interface.  Implement a @racket[live-zombie%]
and @racket[dead-zombie%] class that both implement your
@racket[zombie<%>] interface; implement a @racket[player%] class that
implements your @racket[player<%>] interface.

 Do not use the functional abstraction recipe.  Instead, if you notice
 code that could be shared between the various classes you've
 designed, design super classes and use inheritance to abstract the
 duplicated code.

 Design a @racket[world%] class for playing the Zombie game that
 interacts with the zombie and player objects only according to the
 interfaces you've designed, i.e. the @racket[world%] class should
 work for @emph{any} objects that correctly implement your zombie and 
 player interfaces.

@subsection{Modulo Zombie!}

 Using your interface design from the previous problem, design a 
 @racket[modulo-player%] class and a @racket[modulo-live-zombie%]
 class that implement the @racket[player<%>] and @racket[zombie<%>]
 interfaces, respectively.

 These alternative implementations should behave as follows: the
 player and the zombies may now "wrap around" on the screen.  If a
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

 Experiment with different combinations of your classes from part 2
 and 3 (only the player can wrap around; only the zombies can wrap
 around; some of the zombies and the player; some of the zombies, but
 not the player, etc.) until you find a combination you like best.
 Write down an expression that launches the game using this
 combination.




