#lang scribble/manual
@(require scribble/eval
	  "../class/utils.rkt"
          racket/sandbox
  	  (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
	  (for-label class/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class/0))
    (the-eval '(require 2htdp/image))
    the-eval))

@title{Designing Classes}

One of the most important lessons of @emph{How to Design Programs} is
that the structure of code follows the structure of the data it
operates on, which means that the structure of your code can be
derived @emph{systematically} from your data definitions.  In this
chapter, we see how to apply the design recipe to design data
represented using classes as well as operations implemented as methods
in these classes.

We've seen various kinds of data definitions:
@itemlist[#:style 'ordered
@item{Atomic: numbers, images, strings, ...}
@item{Compound: structures, posns, ...}
@item{Enumerations: colors, key events, ...}
@item{Unions: atoms, ...}
@item{Recursive unions: trees, lists, matryoshka dolls, s-expressions, ...}
@item{Functions: infinite sets, sequences, ...}]

Each of these kinds of data definitions can be realized with objects.
In this chapter, we'll examine how each the first five are implemented
with a class-based design.  We'll return to representing functions
later.

@section{Atomic and Compound Data}

We already saw how to program with the object equivalent of atomic
data in the @secref["Object_=_Data_+_Function"] chapter.  If you
worked through the @secref["Complex_with_class"] exercise, you've
already seen how to program with compound data, too.

Stepping back, we can see that the way to represent some fixed number
@emph{N} of data is with a class with @emph{N} fields.  For example, a
position can be represented by a pair (@emph{x},@emph{y}) of real
numbers:

@#reader scribble/comment-reader
(racketblock
;; A Posn is (new posn% Real Real)
(define-class posn%
  (fields x y))
)


Methods can compute with any given arguments and the object that
calling the method, thus the template for a @racket[posn%] method is:

@#reader scribble/comment-reader
(racketblock
 ;; posn%-method : Z ... -> ???
 (define (posn%-method z ...)
   ... (send this x) (send this y) z ...))

Here we see that our template lists the available parts of the
@racket[posn%] object, in particular the two fields @racket[x] and
@racket[y].

@section{Enumerations}

An @deftech{enumeration} is a data definition for a finite set of
possibilities.  For example, we can represent a traffic light like the
ones on Huntington Avenue with a finite set of symbols, as we did in
Fundies I:

@codeblock[#:keep-lang-line? #f]{
#lang class/0
;; A Light is one of:
;; - 'Red
;; - 'Green
;; - 'Yellow
}

Following the design recipe, we can construct the template for
functions on @tt{Light}s:

@#reader scribble/comment-reader
(racketblock
 ;; light-function : Light -> ???
 (define (light-function l)
   (cond [(symbol=? 'Red l) ...]
	 [(symbol=? 'Green l) ...]
	 [(symbol=? 'Yellow l) ...]))
 )

Finally, we can define functions over @tt{Light}s, following the template.  

@#reader scribble/comment-reader
(racketblock
 ;; next : Light -> Light
 ;; Next light after the given light
 (check-expect (next 'Green) 'Yellow)
 (check-expect (next 'Red) 'Green)
 (check-expect (next 'Yellow) 'Red)
 (define (next l)
   (cond [(symbol=? 'Red l) 'Green]
	 [(symbol=? 'Green l) 'Yellow]
	 [(symbol=? 'Yellow l) 'Red])))

That's all well and good for a function-oriented design, but we want
to design this using classes, methods, and objects.


There are two obvious possibilities.  First, we could create a
@racket[light%] class, with a field holding a @racket[Light].
However, this fails to use classes and objects to their full
potential.  Instead, we will design a class for each state the traffic
light can be in.  Each of the three classes will have their own
implementation of the @racket[next] method, producing the appropriate
@tt{Light}.

@#reader scribble/comment-reader
(racketmod
  class/0
  ;; A Light is one of:
  ;; - (new red%)
  ;; - (new green%)
  ;; - (new yellow%)

  (define-class red%
    ;; next : -> Light
    ;; Next light after red
    (check-expect (send (new red%) next) (new green%))
    (define (next)
      (new green%)))

  (define-class green%
    ;; next : -> Light
    ;; Next light after green
    (check-expect (send (new green%) next) (new yellow%))
    (define (next)
      (new yellow%)))

  (define-class yellow%
    ;; next : -> Light
    ;; Next light after yellow
    (check-expect (send (new yellow%) next) (new red%))
    (define (next)
      (new red%))))

If you have a @tt{Light}, @racket[L], how do you get the next light?

@racket[(send L next)]

Note that there is no use of @racket[cond] in this program, although
the previous design using functions needed a @racket[cond] because the
@racket[next] function has to determine @emph{what kind of light is
the given light}.  However in the object-oriented version there's no
use of a @racket[cond] because we ask an object to call a method; each
kind of light has a different @racket[next] method that knows how to
compute the appropriate next light.  Notice how the purpose statements
are revised to reflect knowledge based on the class the method is in;
for example, the @racket[next] method of @racket[yellow%] knows that
this light is yellow.


@section{Unions and Recursive Unions}

@deftech{Unions} are a generalization of enumerations to represent
infinite families of data.  One example is @emph{binary trees}, which can contain arbitrary other data as elements.  We'll now look at how to model binary trees of numbers, such as:

@verbatim[#:indent 2]{
          7         6              8  
     	           / \            / \ 
                  8   4          2   1
	             / \ 
	            3   2}

How would we represent this with classes and objects?

@#reader scribble/comment-reader
(racketmod
class/0
;;   +- - - - - - - - - - - - - - +
;;   | +- - - - - - - - - - - - + |
;;   V V                        | |
;; A BT is one of:              | |
;; - (new leaf% Number)         | |
;; - (new node% Number BT BT)   | |
;;                     |  +- - -+ |
;;                     +- - - - --+
(define-class leaf%
  (fields number))

(define-class node%
  (fields number left right))

(define ex1 (new leaf% 7))
(define ex2 (new node% 6
		 (new leaf% 8)
		 (new node% 4
		      (new leaf% 3)
		      (new leaf% 2))))
(define ex3 (new node% 8
		 (new leaf% 2)
		 (new leaf% 1)))
)

We then want to design a method @racket[count] which produces the
number of numbers stored in a @tt{BT}.  

Here are our examples:

@racketblock[
(check-expect (send ex1 count) 1)
(check-expect (send ex2 count) 5)
(check-expect (send ex3 count) 3)
]

Next, we write down the
templates for methods of our two classes.

The template for @racket[leaf%]:

@#reader scribble/comment-reader
(filebox 
 (racket leaf%)
 (racketblock
  ;; count : -> Number
  ;; count the number of numbers in this leaf
  (define (count)
    ... (send this number) ...)))

The template for @racket[node%]:

@#reader scribble/comment-reader
(filebox 
 (racket node%)
 (racketblock
  ;; count : -> Number
  ;; count the number of numbers in this node
  (define (count)
    (send this number) ...
    (send (send this left) count) ...
    (send (send this right) count) ...)))

Now we provide a definition of the @racket[count] method for each of
our classes.

@#reader scribble/comment-reader
(filebox 
 (racket leaf%)
 (racketblock
  ;; count : -> Number
  ;; count the number of numbers in this leaf
  (define (count)
    1)))

@#reader scribble/comment-reader
(filebox 
 (racket node%)
 (racketblock
  ;; count : -> Number
  ;; count the number of numbers in this node
  (define (count)
    (+ 1
       (send (send this left) count)
       (send (send this right) count)))))

Next, we want to write the @racket[double] function, which takes a
number and produces two copies of the @tt{BT} with the given number at
the top.  Here is a straightforward implementation for @racket[leaf%]:

@#reader scribble/comment-reader
(filebox 
 (racket leaf%)
 (racketblock
  ;; double : Number -> BT
  ;; double this leaf and put the number on top
  (define (double n)
    (new node%
	 n
	 (new leaf% (send this number))
	 (new leaf% (send this number))))))

Note that @racket[(new leaf% (send this number))] is just constructing a
new @racket[leaf%] object just like the one we started with.
Fortunately, we have a way of referring to ourselves, using the
identifier @racket[this].  We can thus write the method as:

@#reader scribble/comment-reader
(filebox 
 (racket leaf%)
 (racketblock
  ;; double : Number -> BT
  ;; double this leaf and put the number on top
  (define (double n)
    (new node% n this this))))

For @racket[node%], the method is very similar:
@margin-note{Since these two methods are so similar, you may wonder if
they can be abstracted to avoid duplication.  We will see how to do
this in a subsequent class.}

@#reader scribble/comment-reader
(filebox 
 (racket node%)
 (racketblock
  ;; double : Number -> BT
  ;; double this node and put the number on top
  (define (double n)
    (new node% n this this))))

The full @tt{BT} code is now:
@#reader scribble/comment-reader
(racketmod
class/0
;;   +----------------------------+
;;   | +------------------------+ |
;;   V V                        | |
;; A BT is one of:              | |
;; - (new leaf% Number)         | |
;; - (new node% Number BT BT)   | |
;;                     |  +-----+ |
;;                     +----------+
(define-class leaf%
  (fields number)
  ;; count : -> Number
  ;; count the number of numbers in this leaf
  (define (count)
    1)

  ;; double : Number -> BT
  ;; double the leaf and put the number on top
  (define (double n)
    (new node% n this this)))

(define-class node%
  (fields number left right)
  ;; count : -> Number
  ;; count the number of numbers in this node
  (define (count)
    (+ 1
       (send (send this left) count)
       (send (send this right) count)))

  ;; double : Number -> BT
  ;; double the node and put the number on top
  (define (double n)
    (new node% n this this)))

(define ex1 (new leaf% 7))
(define ex2 (new node% 6
		 (new leaf% 8)
		 (new node% 4
		      (new leaf% 3)
		      (new leaf% 2))))
(define ex3 (new node% 8
		 (new leaf% 2)
		 (new leaf% 1)))

(check-expect (send ex1 count) 1)
(check-expect (send ex2 count) 5)
(check-expect (send ex3 count) 3)

(check-expect (send ex1 double 5)
	      (new node% 5 ex1 ex1))
(check-expect (send ex3 double 0)
	      (new node% 0 ex3 ex3)))

@include-section{sec-more-rocket.scrbl}

@section[#:tag "Exercises (Ch. 2)"]{Exercises}

@subsection{Lists}

Design classes to represent lists of numbers.  Implement the methods
@tt{length}, @tt{append}, @tt{sum}, @tt{prod}, @tt{contains?},
@tt{reverse}, @tt{map}, and @tt{max}.  Note that @tt{max} raises some
interesting design decisions in the case of the empty list.  One
solution is to define the @tt{max} of the empty list as negative
infinity, @racket[-inf.0], a number smaller than every other number
(except itself).

@subsection[#:tag "assign_range"]{Home on the Range}

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

@subsection{Zombie!}
       
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

@subsection{Primum non copy-and-paste}

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