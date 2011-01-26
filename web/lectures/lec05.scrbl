#lang scribble/manual
@(require "../utils.rkt"
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class1 define-struct ... length))
	  (for-label 2htdp/image)
	  (for-label class1/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class1))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@title[#:tag "lec05"]{1/24: Larger system design: Snakes on a plane}

@internal{
NOTES FOR FUTURE VERSIONS OF THIS LECTURE

1) Make sure to get a whole subsystem working first (e.g., can draw
snakes and "tick" snakes.  Then add death.  Then add eating and
growth.  Then add mouse handling and changing direction.)

2) Construction should be considered part of the interface.}


@section[#:tag-prefix "lec05"]{Announcements}
@itemlist[


@item{Van Horn, Tobin-Hochstadt, and Brown will all be in lovely
Austin, Texas for the remainder of the week in order to participate in
the @link["http://popl/"]{ACM SIGPLAN/SIGACT Conference on the Principles of
Programming Languages}.  Tobin-Hochstadt will have limited office
hours today and Brown and Van Horn will not have office hours this
week.  There will be a lecture on Thursday, given by Vincent
St. Amour, a PhD candidate in the Programming Research Lab.  Come
prepared with good Canadian jokes.}

@item{Just a reminder: there will be a partner swap next Wednesday
that will go into effect for assignment 4.  If you would like to
request a partner, send us email @emph{before} Wednesday.  If we don't
receive email from you, we will assign you a partner randomly.}
]

@section[#:tag-prefix "lec05"]{Questions and answers}

@itemlist[

@item{Q: I've designed a single interface @racket[being<%>] that
subsumes both @racket[zombie<%>] and @racket[player<%>] in the current
assignment.  Do I still have to design a @racket[zombie<%>] and
@racket[player<%>] interface?

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
can write a @racket[being<%>] interface.  You still need to write down
the @racket[zombie<%>] and @racket[player<%>] interfaces, but if
you're careful, you may be able to arrange things so that your classes
that implement zombies and players declare to implement the
@racket[being<%>] interface which @emph{implies} they also implement
the @racket[zombie<%>] and @racket[player<%>] interface, respectively.
We will cover this topic in more depth next Monday.}

@item{Q: Interfaces often have overlapping sets of behaviors. For
example, zombies and players share much of the same functionality.  Is
there a way to do abstraction at the interface level?  

A: In @racketmodname[class1], there is currently no way to express
this kind of abstraction.  We will consider adding this feature and
covering it in future lectures.}

@item{Q: Which is considered a better design: a union with two
variants, or a single variant with a Boolean field that indicates
"which part of the union this data belongs to"?  For example, is it
better to have a @racket[live-zombie%] and @racket[dead-zombie%] class
or a single @racket[zombie%] class with a @racket[dead?] field.

A: One of the themes of this and last semester is that @emph{once you
have settled on choice for the representation of information in your
program, the structure of your program follows the structure of that
data}.  We've trained you to systematically derive code structure from
data structure; there's a recipe---you don't even have to think.
That's great because it frees up significant quantities of the most
precious and limited resource in computing: your brain.  But
unfortunately, that recipe kicks in only after you've chosen how
information will be represented, i.e. after you've written down data
definitions.  Much of the hard work in writing programs, and where
your creative energy and brain power is really needed, is going from
information to representation.  There is no recipe for this part of
program design.  You need to develop the ability to analyze problems,
take an account of what knowledge needs to be represented to solve a
problem, and practice making decisions about how that knowledge can be
represented computationally.  The good news is you don't have to be
struck by divine inspiration to manage this step.  Program design is a
process of iterative refinement: make some choices, follow the recipe.
You will discover ways to improve your initial choices, so go back and
revise, then carry out the changes in code structure those design
decision entail.  Rinse and repeat.

This is a long winded way of saying: there is no universal "right"
answer to this question.  It will depend on the larger context.  That
said, there are some important things to take into account for this
particular question.  It is much easier to add new variants to a union
than it is to create Boolean values other than @racket[true] and
@racket[false].  Good program design is often based on anticipating
future requirements.  If you think it's possible that there might be
some other kind of zombie down the road, the union design will be less
painful to extend and maintain.}

@item{Q: The @racket[define-interface] mechanism is a way of enforcing
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
tool for writing programs}.}
]

@section{Information in the Snake Game}

In the @seclink["lec04"]{previous lecture}, we introduced a lot of
important new concepts such as interfaces and data and method
inheritance for class-based abstraction.  Today we are going to see
these concepts being applied in the context of a larger program design
that we will develop together.  It's a game you've all seen and designed
before; we're going to develop a class-based version of the Snake
Game.

Our first task in designing the Snake Game is to take an account of
the information that our program will need to represent.  This
includes:

@itemlist[
 @item{a system of coordinates}
 @item{a snake, which has
   @itemlist[
     @item{direction}
     @item{segments}]}
 @item{food}
 @item{a world}]

@section{Coordinate interface}

Let's focus on the system of coordinates. There are really two
coordinate systems we will need to represent; one is necessitated by
the animation system we are using, the other by the logic of the Snake
Game.  We are @emph{consumers} of the @racket[big-bang] animation
system, which uses a pixel-based, graphics-coordinate system (meaning
the origin is at the Northwest corner).  This is part of
@racket[big-bang]'s interface, which we don't have the power to
change, and therefore we have to communicate to @racket[big-bang]
using pixels in graphics-coordinates.  In general, when we design
programs, the interfaces of libraries we use impose obligations on our
code.

On the other hand, using pixel-based graphics-coordinates is probably
not the best representation choice for the information of the Snake
Game.  We'll be better off if we design our own representation and
have our program translate between representations at the
communication boundaries between our code and @racket[big-bang].
Let's use a grid-based coordinate system where the origin is the
Southwest corner.  We can define a mapping between the coordinate
systems by defining some constants such as the grid size and the 
size of the screen:

@#reader scribble/comment-reader
(racketmod
  class1
  (define WIDTH 512) ; in pixels
  (define HEIGH 512) ; in pixels
  (define SIZE 16)   ; in pixels / grid unit
)

This defines that our game is logically played on a 32x32 grid, which we
will render visually as a 512x512 pixel image.  This are the values we
cooked up in class, which results in the following grid for our game:

@(the-eval 
  '(begin
     (require 2htdp/image)
     (require (only-in racket for*/fold in-range))
     
     (define SIZE 16)
     (define WIDTH 512)
     (define HEIGHT 512)
     
     (define (grid->px-x x)
       (* SIZE (+ 1/2 x)))
     
     (define (grid->px-y y)
       (- HEIGHT (* SIZE (+ 1/2 y))))
     
     (check-expect (grid->px-x 0) (* 1/2 SIZE))
     (check-expect (grid->px-y 0) (- HEIGHT (* 1/2 SIZE)))
     
     (define (add-sq x y scn)
       (place-image (overlay (text (string-append (number->string x)
						  ","
						  (number->string y))
				   6
				   "black")
			     (square SIZE "outline" "black"))
		    (grid->px-x x)
		    (grid->px-y y)
		    scn))
     
     (define MT-SCENE (empty-scene WIDTH HEIGHT))
     
     (for*/fold ([scn MT-SCENE])
       ([i (in-range 0 (/ WIDTH SIZE))]
	[j (in-range 0 (/ HEIGHT SIZE))])
       (add-sq i j scn))))

But for the sake of our notes, let's develop the game for a much
smaller grid that is rendered on a larger scale.  It will be easy to
change our definitions at the end in order to recover the original
design.  If done properly, all of test cases will remain unaffected by
the change.

@#reader scribble/comment-reader
(racketmod
  class1
  (define WIDTH 256) ; in pixels
  (define HEIGH 256) ; in pixels
  (define SIZE 32)   ; in pixels / grid unit
)

This defines that our game is logically played on a 8x8 grid, which we
will render visually as a 256x256 pixel image.  The grid for our game
now looks like:

@(the-eval 
  '(begin     
     (define SIZE 32)
     (define WIDTH 256)
     (define HEIGHT 256)
     (define (grid->px-x x)
       (* SIZE (+ 1/2 x)))     
     (define (grid->px-y y)
       (- HEIGHT (* SIZE (+ 1/2 y))))     
     (define (add-sq x y scn)
       (place-image (overlay (text (string-append (number->string x)
						  ","
						  (number->string y))
				   12
				   "black")
			     (square SIZE "outline" "black"))
		    (grid->px-x x)
		    (grid->px-y y)
		    scn))
     
     (define MT-SCENE (empty-scene WIDTH HEIGHT))
     
     (for*/fold ([scn MT-SCENE])
       ([i (in-range 0 (/ WIDTH SIZE))]
	[j (in-range 0 (/ HEIGHT SIZE))])
       (add-sq i j scn))))

Now we need to consider the interface for coordinates (henceforth, the
term "coordinate" refers to @emph{our} representation of coordinates
in the Snake Game, not the graphics-coordinates of @racket[big-bang]).
What do we need to do with coordinates?  Here's a coarse first
approximation:

@itemlist[
@item{Compare two coordinates for equality.}
@;item{Convert to pixel-based graphics-coordinates.}
@item{Draw something at a coordinate on to a scene.}
@item{Move a coordinate.}
@item{Check that a coordinate is on the board.}
]

This list suggest the following interface for coordinates:

@#reader scribble/comment-reader
(racketblock
  ;; A Coord implements coord<%>.
  (define-interface coord<%>
    [;; Coord -> Boolean
     ;; Is this coordinate at the same position as the given one?
     compare
     ;; Scene -> Scene
     ;; Draw this coordinate on the scene.
     draw 
     ;; -> Coord
     ;; Move this coordinate.
     move
     ;; -> Boolean
     ;; Is this coordinate on the board?
     check]))

This is a good place to start, but as we start thinking about
@emph{what} these methods should do and @emph{how} we might write
them, some issues should come to mind.  For example, in thinking about
the what: what should be drawn when the @racket[draw] method is
invoked?  Perhaps we want the coordinate "to just know" what should be
drawn, which suggests that when we implement coordinates they should
contain data representing what to draw.  Perhaps we want to tell the
@racket[draw] method what to draw, which suggests we should revise the
contract to include an argument or arguments that represent what to
draw.  For the time-being, let's decide that the coordinate will know
what to draw.

In the thinking about the how: how do you imagine the draw coordinate
will be written?  Assuming we know what to draw, the next question is
how will the method know where to draw it?  We have a coordinate,
which is a grid coordinate, but will need to use @racket[place-image]
to actually draw that image on the given scene.  But
@racket[place-image] works in the pixel-based graphics-coordinate
system.  We need to be able to convert a coordinate to a pixel-based
graphics-coordinate in order to write the @racket[draw] method, but
there is nothing in the interface that gives us that capability,
@emph{and the interface is all we will have to work with}.  This
suggests we should revise the interface to include this needed
behavior.

Similarly, if we consider how to write the @racket[compare] method, we
will want to compare the @emph{x}- and @emph{y}-components of the
given coordinate with the @emph{x}- and @emph{y}-components of this
coordinate.  Again, there is nothing in the interface as given that
allows this, so we need to revise.

Now consider the @racket[move] method.  How can we write it?  What do
we expect to happen?  There's not enough information to know---should
the coordinate move up?  Down?  Right three and down seven?  Hard to
say.  While we might expect a coordinate to know how to draw itself,
we cannot expect a coordinate to know which way to move itself.  This
suggests we need to add inputs to the method that represent this
needed information.  For the purposes of our game, a position needs to
be able to move one grid unit in one of four directions.  Let's design
the representation of a direction and a directional input to the
@racket[move] method.

Finally, the names @racket[check] and @racket[compare] are less
informative than they could be.  The @racket[compare] method answers
the question "are two coordinates at the same position?", so let's
instead call it @racket[same-pos?].  The @racket[check] method answers
the question "is this coordinate on the board?", so let's instead call
it @racket[on-board?].

Interface design is incredibly important, especially when, unlike in
our current situation, it is not easy to revise in the future.  Modern
computer systems are littered with detritus of past interface design
choices because interfaces are difficult and expensive, if not
impossible, to change.  As an example, the developers of the UNIX
operating system, which was developed in the 1970s and is now the
basis of both Linux and Mac OS, made the choice to save characters and
call the operation that creates a file "CREAT".  Forty years later,
I'm writing these notes on a portable computer while flying from
Boston to Houston.  My machine, which weighs far less than any
computer that ran UNIX in the 70's, has not one, but two 2.66 GHz
processors and 8 gigs of RAM: unimaginable computing resources in the
70s.  And yet, when my OS wants to make a new file, it calls the
"CREAT" function---not because that missing "E" is a computational
extravangance I cannot afford, far from it, but because it is simply
too difficult a task to realize a redesign of the interface between my
computer and its operating system.  The unforunate thing about
interfaces is that when they change, all parties that have agreed to
that interface must change as well.  Too many people, programs, and
devices have agreed to the UNIX interface to make changing "CREAT"
to "CREATE" worthwhile.

You won't be able to make the perfect interface on the first try, but
the closer you get, the better your life will be in the future.

Revising our interface as described above, we arrive at the following:

@#reader scribble/comment-reader
(racketblock
  ;; A Coord implements coord<%>.
  (define-interface coord<%>
    [;; Coord -> Boolean
     ;; Is this coordinate at the same position as the given one?
     same-pos?
     ;; Scene -> Scene
     ;; Draw this coordinate on the scene.
     draw 
     ;; Dir -> Coord
     ;; Move this coordinate in the given direction.
     move
     ;; -> Boolean
     ;; Is this coordinate on the board?
     on-board?
     ;; -> Nat
     ;; The {x,y}-component of grid-coordinate.
     x y
     ;; -> Nat
     ;; The {x,y}-component of pixel-graphics-coordinate.
     x-px y-px]))

We haven't designed @tt{Dir} data definition for representing
direction; let's take care of that quickly.  In our game, a direction
is one of four possibilities, i.e. it is an enumeration.  We could use
a class-based enumeration, but for the sake of simplicity, let's 
just use strings and say that:

@#reader scribble/comment-reader
(racketblock
  ;; A Dir is one of:
  ;; - "left"
  ;; - "right"
  ;; - "up"
  ;; - "down"
)

This representation has the nice property of being a subset of
@racket[big-bang]'s @tt{KeyEvent} representation, so we can rely on
the coincidence and handle the "up" key event by moving in the "up"
direction without need to convert between representations.

@section{An implementation of coordinates: segments}

At this point, we've flushed out enough of the initial design of the
coordinate interface we can now start working on an implementation of
it.  There are two components that will implement the coordinate
interface: segments and food.  Let's start with segments.

@#reader scribble/comment-reader
(racketblock
  ;; A Segment is a (new seg% Nat Nat)
  ;; Interp: represents a segment grid-coordinate.
  (define-class seg%
    (implements coord<%>)
    (fields x y))
)

Our template for @racket[seg%] methods is:

@#reader scribble/comment-reader
(racketblock
  ;; ?? -> ??
  (define/public (seg-template ...)
    (field x) ... (field y) ...)
)

We've now made a data definition for segments and committed ourselves
to implementing the interface.  This obligates us to implement all of
the methods in @racket[coord%].  We've decided to implement the
@racket[coord%] using a class with an @racket[x] and @racket[y] field.
This satisfies part of our implementation right off the bat: we get an
@racket[x] and @racket[y] method by definition.  Let's now do the
@racket[x-px] and @racket[y-px] methods:







@internal{

Interfaces avoid commitments to representation choices.  So long as we
implement the interface, we can make our own representation choices.

An implementation of coord<%>, segment%:

(implements coord%) - commits us to implementing a bunch of methods.

- check:

First let's write some tests.

(check-expect (send (new segment% 100 100) check) false)
(check-expect (send (new segment% 1 2) check) true)

Template:

(define/public (check)
  ... (field x) ... (field y) ...)

Code:

;; In segment%:
(define/public (check)
  (and (< 0 (field x) GRIDW)
       (< 0 (field y) GRIDH)))

Is there anything wrong with this code?  Let's think about the grid
coordinate system.

Is (0,0) a legal coordinate?  Yes.

Is `check' allowing.

Choose good names.

Change `check' to `in-bounds?'.  Requires a change to the interface
which is oustide our domain of control (well, not really, we're
writing this whole program).

Interface design is one of the hardest problems in computer science.

Unfortunately, the only way to avoid all these problems is to get it
100% right at the very beginning and unfortunately there is no way to
get it 100% right at the very beginning.

The CREAT story.

We can use aspects of class1 that aren't about classes.  Write a
helper function for abstracting the grid to pixel conversion.

Compare
=======

...

Move
====

The Direction data defintion

A Direction is one of:
- "left"
- "right"
- "up"
- "down"

A Direction is one of:
- (make-posn 0 +1)
- (make-posn 0 -1)
- (make-posn +1 0)
- (make-posn -1 0)

Can we abstract the `move' method?

There is no abstraction mechanism in our language that helps here.

WORLDS
======

;; A World is a (new world% Snake Food Direction ...)

vs

;; A World is a (new world% Snake Food ...).
;; [where the Snake has a direction]

Food
====

Is a coordinate.

Snake
=====

;; A Snake is a (new snake% ...)

What information needs to be represented in a snake?

- Direction
- List of segments

(define-class snake%
  (field segments dir))

What operations do we need?

- tick : ??

But what does the snake need to do on tick?

Alternative:

move : -> Snake
grow : -> Snake
turn [change direction] : Direction -> Snake

(Or, do we change direction externally by constructing a new snake).

We can make a choice about operations or doing things externally.

;; move

(define/public (snake-template ...)
  (field segments) ... (field dir) ...)

(define/public (move)
  (new snake% ... ...))

What's the new direction?  Same as before.

(define/public (move)
  (new snake% ... (field dir)))

What's the new list of segments? 

(define/public (move)
  (new snake% 
       (cons (send ... move ...)
             ...missing tail...)
       (field dir)))

(define/public (move)
  (new snake% 
       (cons (send (first (field segments) move (field dir))
             ...missing tail...)
       (field dir)))

;; leave drop-last as helper to be written.

Revised interpretation on order of segments.
Revised data definition to non-empty list of segments.

;; grow -- a lot like move, but without the missing tail.

What are we going to do on every turn?

;; draw 

fold over the list of segments and send the draw method to each
segment.

Shows functions and objects can fit together nicely.

Writing the world
=================

to-draw : -> Scene

Draw the snake and the food.

We still haven't implemented food, but we know how to draw a food.  We
just can't create a food.

on-tick : -> World

For the moment have it handling eating.

Food
====

...


--------------

Most important: designing different peices of your program independently

What are the pieces?  How can they communicate?

}