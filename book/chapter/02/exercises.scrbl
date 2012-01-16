#lang scribble/manual
@(require class/utils)

@title[#:tag "Exercises (Ch. 2)"]{Exercises}

@section{Lists}

Design classes to represent lists of numbers.  Implement the methods
@tt{length}, @tt{append}, @tt{sum}, @tt{prod}, @tt{contains?},
@tt{reverse}, @tt{map}, and @tt{max}.  Note that @tt{max} raises some
interesting design decisions in the case of the empty list.  One
solution is to define the @tt{max} of the empty list as negative
infinity, @racket[-inf.0], a number smaller than every other number
(except itself).

@section[#:tag "assign_range"]{Home on the Range}

A @emph{range} represents a set of numbers between two endpoints.  To
start with, you only need to consider ranges that @emph{include} the
smaller endpoint and @emph{exclude} the larger endpoint---such ranges
are called @emph{half-open}.  For example, the range [3,4.7) includes
all of the numbers between 3 and 4.7, including 3 but @emph{not}
including 4.7. So 4 and 3.0000001 are both in the range, but 5 is
not.  In the notation used here, the ``['' means include, and the ``)''
means exclude.


@itemlist[

@item{Design a representation for ranges and implement the
  @r[in-range?] method, which determines if a number is in the
  range.  For example, the range [3,7.2) includes the numbers 3
  and 5.0137, but not the numbers -17 or 7.2.}

@item{Extend the data definition and implementation of ranges to
  represent ranges that @emph{exclude} the low end of the range and
  @emph{include} the high end, written (lo,hi].}

@item{Add a @r[union] method to the interface for ranges and implement
  it in all range classes.  This method should consume a range and
  produces a new range that includes all the numbers in this range
  @emph{and} all the numbers in the given range.

  You may extend your data definition for ranges to
  support this method.
  
  Don't worry if your initial design duplicates code; you can abstract
  later.
}
]

@section{Zombie!}
       
Design and develop an interactive game called @emph{Zombie!}.  In this
game, there are a number of zombies that are coming to eat your
brains.  The object is simple: stay alive.  You can maneuver by moving
the mouse.  The player you control always moves toward the mouse
position.  The zombies, on the other hand, always move toward you.  If
the zombies ever come in contact with you, they eat your brains, and
you die.  If two zombies happen to come in to contact with each other,
they will mistakenly eat each other's brain, which it turns out is
fatal to the zombie species, and so they both die.  When a zombie
dies, the zombie flesh will permanently remain where it is and should
any subsequent zombie touch the dead flesh, they will try to eat it
and therefore die on the spot.  Survive longer than all the zombies,
and you have won the game.

(This game is based on the @emph{Attack of the Robots!} game described
in @link["http://landoflisp.com/"]{Land of Lisp}.  Unlike the Land of
Lisp version, this game is graphical and interactive rather than
text-based.  Hence, our game doesn't suck.)

You can play the game by running:

@#reader scribble/comment-reader
(racketmod
class/0
(require class/0/zombie)
(require class/universe)
(big-bang zombie!)
)

Once you have a working version of the game, add the following
feature: whenever the user does a mouse-click, the player should be
instantly teleported to a @emph{random} location on the screen.

@section{Primum non copy-and-paste}

A natural design for the Zombie game is to have a @tt{Zombie} and
@tt{Player} class of data.  But you may find your first iteration of
the Zombie game duplicates a lot of code between these classes.  In
fact, the @tt{Zombie} and @tt{Player} classes have more in common than
apart.  It may even be tempting to pursue an unnatural design in which
there is only a single class of data, which must consist of an
additional @emph{bit}, which is interpreted as signifying "zombieness"
versus "playerness".  Down that path waits shame, defeat, and a
brittle design that makes babies cry.

To recoil at the prospect of copy-and-paste is commendable, but we
shouldn't throw the crying babies out with the bathwater.  Let's step
back and ask ourselves if this dilemma is really inescapable.

First, let's consider the information that needs to be represented in
a game.  When you look at the game, you see several things: live
zombies, dead zombies, a player, and a mouse.  That might lead you to
a representation of each of these things as a separate class, in which
case you may later find many of the methods in these classes are
largely identical.  You would like to abstract to avoid the code
duplication, but thus far, we haven't seen any class-based abstraction
mechanisms.  So there are at least two solutions to this problem:

@itemlist[
 @item{Re-consider your data definitions.

 Program design is an iterative process; you are continually revising
 your data definitions, which induces program changes, which may cause
 you to redesign your data definitions, and so on.  So when you find
 yourself wanting to copy and paste lots of code, you might want to
 reconsider how you're representing information in your program.  In
 the case of zombie, you might step back and see that although the
 game consists of a player, dead zombies, live zombies, and a mouse,
 these things have much in common.  What @emph{changes over time}
 about each of them is their position.  Otherwise, what makes them
 different is how they are rendered visually.  But it's important to
 note that way any of these things are rendered @emph{does not change
 over the course of the game}---a dead zombie is @emph{always} drawn
 as a gray dot; a live zombie is @emph{always} drawn as a green dot;
 etc.  Taking this view, we can represent the position of each entity
 uniformly using a single class.  This avoids duplicating method
 definitions since there is only a single class to represent each of
 these entities.
 }
 @item{Abstract using the functional abstraction recipe of last semester.

 Just because we are in a new semester and studying a new paradigm of
 programming, we should not throw out the lessons and techniques we've
 previously learned.  In particular, since we are writing programs in
 a multi-pararadigm language---one that accomodates both structural
 and functional programming @emph{and} object-oriented
 programming---we can mix and match as our designs necessitate.  In
 this case, we can apply the recipe for @emph{functional abstraction}
 to the design of identical or similar methods, i.e. two methods with
 similar implementations can be abstracted to a single point of
 control by writing a helper function that encapsulates the common
 code.  The method can then call the helper function, supplying as
 arguments values that encapsulate the differences of the original
 methods.  } #:style 'ordered ]

Revise your Zombie! program.

@include-section[(lib "assignments/assign02-space-invaders.scrbl")]