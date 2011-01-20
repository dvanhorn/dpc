#lang scribble/manual
@(require "../utils.rkt"
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class1 define-struct ... length))
	  (for-label class1/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class1))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@title[#:tag "lec04"]{1/20: Inheritance and interfaces}


@section[#:tag-prefix "lec04"]{Announcements}
@itemlist[

@item{Solutions to homeworks are going to be made available on the
course web page.  The solutions to @seclink["soln01"]{assignment 1}
and @seclink["soln02"]{assignment 2} are already up.  This should not
be taken as @emph{the one and only true} solution to the assignments;
they simply represent a single design.}

@item{Assignment 2 was due last night and assignment 3 will be out
later today and is due Wednesday night.}

@item{A new programming language, @racketmodname[class1], has been
released.  Be sure to download and install the latest @tt{.plt} file.}

@item{Books are in, so be sure to keep up on the reading.  You should
be reading with an emphasis on the @emph{design concepts} covered in
the book, not on the particulars of the Java programming language.  We
will cover Java in class, but until then, you are not responsible for
knowing how to program in Java (although it is not a bad idea to pick
it up as you go).}]

@section{Discussion of assignment 2}

@subsection{Lists}

On the subject of the list finger exercises, we got email like this:

@nested[#:style 'inset]{ We are not allowed to use
@racketmodname[class0] lists; are we allowed to use @racket[null] (aka
@racket[empty]), or must we create our own version of that too? }

and:

@nested[#:style 'inset]{
Should my functions also apply for empty lists (i.e. have empty lists
be represented as objects), or could we simply use the built-in
@racket[empty] datatype (on which none of my methods would work)?}

There are two things worth noting, one is important and one is not:
@itemlist[

 @item{The @racket[empty] value is a list and the assignment
 specifically prohibited using lists; so even if you could use
 @racket[empty] to represent the empty list, this wouldn't live up to
 the specification of the problem.}

 @item{You won't be able to represent the empty list with a value that
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
   (define/public (method-template ...)
     (field first) ...
     (send (field rest) method-template ...)))
]

Notice that the method is sent to the rest of this list.  If the rest
of the list is empty, then this will break unless the empty list is an
object, and moreover, an object that understands @racket[method-template].}

#:style 'ordered]

@subsection{Zombie}

On the subject of the zombie game, we got a bunch of emails like this:

@nested[#:style 'inset]{ My partner and I have come upon a dilemma in
Homework 2 in Honors Fundies 2 because we have not learned inheritance
yet.  We can have two separate classes Player and Zombie and tie them
together in a union called Being, but that'll result in some
significant copy-and-paste code.  The alternative is to make one class
called Being and in the *single* function that differs between players
and zombies, use a @racket[cond] based on a field (@racket['zombie] or
@racket['player]).  We were told not to do this, but we were also told
copy-and-pasting code is bad...hence the dilemma.}

and:

@nested[#:style 'inset]{ My partner and I have been working through
the current assignment and with our representation of zombies and
players there is a lot of reused code.  I was wondering if there is
some form of inheritance in the @racketmodname[class0] language that
has not been covered.  I think that inheritance would make our current
code much cleaner by solving the problem of reused code.  Is there a
way to implement inheritance in the @racketmodname[class0] language
currently?}


One of the subjects of today's lecture is inheritance, however let's
step back and ask ourselves if this dilemma is really as inescapable
as described.

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

That said, one of the subjects of today's lecture will be an
object-oriented abstraction mechanism that provides a third
alternative to resolving this issue.

@section{Inheritance}

@subsection{Method and data inheritance with binary trees}

Last time, we developed classes for representing binary trees and
wrote a couple methods for binary trees.  The code we ended with was:

@#reader scribble/comment-reader
(racketmod
  class0
  ;; A BT is one of:
  ;; - (new leaf% Number)
  ;; - (new node% Number BT BT)
  
  (define-class leaf%
    (fields number)
    ;; -> Number
    ;; count the number of numbers in this leaf
    (define/public (count)
      1)
    ;; Number -> BT
    ;; double the leaf and put the number on top
    (define/public (double n)
      (new node% n this this)))
  
  (define-class node%
    (fields number left right)
    ;; -> Number
    ;; count the number of numbers in this node
    (define/public (count)
      (+ 1
	 (send (field left) count)
	 (send (field right) count)))
    ;; Number -> BT
    ;; double the node and put the number on top
    (define/public (double n)
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
		(new node% 0 ex3 ex3))
  )

One of the troublesome aspects of this code is the fact that the two
implementations of @racket[double] are identical.  If we think by
analogy to the structural version of this code, we have something like
this:

@#reader scribble/comment-reader
(racketmod
 class0
 ;; A BT is one of:
 ;; - (make-leaf Number)
 ;; - (make-node Number BT BT) 
 (define-struct leaf (number))
 (define-struct node (number left right))
 
 ;; BT Number -> BT
 ;; Double the given tree and put the number on top.
 (define (double bt n)
   (cond [(leaf? bt) (make-node n bt bt)]
	 [(node? bt) (make-node n bt bt)]))
 )

We would arrive at this code by developing the @racket[double]
function according to the design recipe; in particular, this code
properly instantiates the template for binary trees.  However, after
noticing the duplicated code, it is straightforward to rewrite this
structure-oriented function into an equivalent one that duplicates no
code.  All cases of the @racket[cond] clause produce the same result,
hence the @racket[cond] can be eliminated, replaced by a single
occurrence of the duplicated answer expressions:

@#reader scribble/comment-reader
(racketblock
 ;; BT Number -> BT
 ;; Double the given tree and put the number on top.
 (define (double bt n)
   (make-node n bt bt))
)

But switching back to the object-oriented version of this code, it is
not so simple to "eliminate the @racket[cond]"---there is no
@racket[cond]!  The solution, in this context, is to abstract the
identical method defintions to a common @emph{super} class.  That is,
we define a third class that contains the method shared among
@racket[leaf%] and @racket[node%]:

@#reader scribble/comment-reader
(racketblock
  (define-class bt%
    ;; -> BT
    ;; Double this tree and put the number on top.
    (define/public (double n)
      (new node% n this this)))
)

The @racket[double] method can be removed from the @racket[leaf%] and
@racket[node%] classes and instead these class can rely on the
@racket[bt%] definition of @racket[double], but to do this we must
establish a relationship between @racket[leaf%], @racket[node%] and
@racket[bt%]: we declare that @racket[leaf%] and @racket[node%] are
@emph{subclasses} of @racket[bt%], and therefore they @emph{inherit}
the @racket[double] method; it is as if the code were duplicated
without actually writing it twice:

@#reader scribble/comment-reader
(racketblock
  (define-class leaf%
    (super bt%)
    (fields number)
    ;; -> Number
    ;; count the number of numbers in this leaf
    (define/public (count)
      1))

  (define-class node%
    (super bt%)
    (fields number left right)
    ;; -> Number
    ;; count the number of numbers in this node
    (define/public (count)
      (+ 1
	 (send (field left) count)
	 (send (field right) count))))
)

To accomodate this new feature---@emph{inheritance}---we need to
adjust our programming language.  We'll now program in
@racketmodname[class1], which is a superset of
@racketmodname[class0]---all @racketmodname[class0] programs are
@racketmodname[class1] programs, but not vice versa.  The key
difference is the addition of the @racket[(super _class-name)] form.

@(the-eval
  '(begin
     ;(define-class bt%) ; pre-bt% -- doesn't work
     (define-class node%
       (super bt%)
       (fields number left right)
       ;; -> Number
       ;; count the number of numbers in this node
       (define/public (count)
	 (+ 1
	    (send (field left) count)
	    (send (field right) count))))

     (define-class leaf%
       (super bt%)
       (fields number)
       ;; -> Number
       ;; count the number of numbers in this leaf
       (define/public (count)
	 1))

     (define-class bt%
       ;; -> BT
       ;; Double this tree and put the number on top.
       (define/public (double n)
	 (new node% n this this)))))

@interaction[#:eval the-eval
(send (new leaf% 7) double 8)
]


@itemlist[
 @item{posn% and shapes:
  data inheritance: a rect% is a posn% + width + height,
                    a circ% is a posn% + radius}
 @item{implement some methods: area, draw-on}

 @item{a look back at binary trees: there was code duplication.
  method inheritance: adding bt%, lifting double method}

 @item{back to shapes: can we lift anything?  No, because the definition of
  draw-on is different.  *But* we can refactor by adding an img method
  that constructs the shape image.  Now draw-on is identical and
  can be lifted.

  There is a subtlety here with method invocation.  In the first
  identical implementations of draw-on, you have calls to (img), but
  when you lift, you want (send this img) because the super class
  does not have an img method. }]


@section{Interfaces}

@subsection{Lights, redux}

@itemlist[
 @item{a look back at light}
 @item{add a draw method}
 @item{make it a world program that cycles through the lights}
 @item{what does it mean to be a light?  next and draw}
 @item{alternative design of light class}
 @item{Q: what changes in world?  A: nothing.}
 @item{motivates the use of interfaces}
 @item{add interface to light design.  both alternatives implement it,
  and the world will work with anything that implements it}]


