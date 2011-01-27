#lang scribble/manual
@(require "../utils.rkt"
          (for-label class1))

@title[#:tag "assign04"]{2/2: Tron}

Due: 2/2.

@itemlist[#:style 'ordered 
 @item{@bold{Tron}

 For this assignment, you will design and develop the Tron Lightcycle game.
 The basic idea of this game, if you haven't seen the movie, is that
 two players move around the screen, leaving a trail of where each of the
 players has been.  If a player runs into the trail that they or the
 other player has left behind, or into a wall, they lose.  If the two
 players simultaneously hit each other, or both hit a wall or trail at
 the same time, the game is a tie.

 Here's a screenshot of playing the game:

 @image["assignments/tron1.png"]

 Here's a
 @link["http://www.classicgamesarcade.com/game/21670/Tron-Game.html"]{flash
 game} where you can play the game yourself online.  }

 @item{@bold{Distributed Tron}
 
 

 }

 @item{@bold{Computer Tron}

 Language: @racketmodname[class1]. 

 }	   

 @item{@bold{Finger exercises: parametric lists}

 Language: @racketmodname[class1]. 

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

}]

