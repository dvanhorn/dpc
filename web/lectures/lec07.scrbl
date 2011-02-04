#lang scribble/manual
@(require "../utils.rkt"
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class1 define-struct ... length))
	  (for-label 2htdp/image)
	  (for-label class1/universe))

@title[#:tag "lec07"]{1/31: Delegation}


@section[#:tag-prefix "lec07"]{Announcements}

@itemlist[
 @item{You have been assigned a new partner, who you should have
 contacted already.}
]

@section{New language features}

@subsection{Interface intheritance}

Interfaces can now inherit from other interfaces.  In this example,
the @r[foo%] class promises to implement the methods specified in
@r[bar<%>], but also the methods listed in the super-interfaces
@r[baz<%>] and @r[foo<%>].

@#reader scribble/comment-reader
(racketmod
  class1
  (define-interface baz<%> (blah))
  (define-interface foo<%> (blah))
  (define-interface bar<%> 
    (super foo<%>)
    (super baz<%>)
    (x y))

  (define-class foo%
    (implements bar<%>)
    (fields x y z blah))
)


@subsection{Dot notation}

To make programming with objects more convenient, we've added new
syntax to @racketmodname[class1] to support method calls.  In
particular, the following now sends method @r[x] to object @r[o] with
argument @r[arg]:

@racketblock[
(x #,(racketidfont ".") o arg)
]

This is equivalent to
@racketblock[
(send x o arg)
]

We can chain method calls like this:

@racketblock[
(x #,(racketidfont ".") o arg #,(racketidfont ".") m arg*)
]

This sends the @r[m] method to the @emph{result} of the previous
expression.  
This is equivalent to
@racketblock[
(send (send x o arg) m arg*)
]

Although in lecture this didn't work in the interactions window, it
now works everywhere that you use @racketmodname[class1]. 


@section{Constructor design issue in modulo zombie (Assignment 3,
Problem 3)}

Course staff solution for regular zombie game:

@filebox["world%"]{
@#reader scribble/comment-reader
(racketblock
 (define/public (teleport)
   (new world%
	(new player%
	     (random WIDTH)
	     (random HEIGHT))
	(field zombies)
	(field mouse)))
)
}

This has a significant bug: it always produces a plain
@racket[player%], not a @racket[modulo-player%].

Bug (pair0MN):

@filebox["modulo-player%"]{
@#reader scribble/comment-reader
(racketblock
 (define/public (teleport)
   (new player%
	(* -1 (random WORLD-SIZE))
	(* -1 (random WORLD-SIZE))))
)
}

This has a similar bug: it always produces a plain
@racket[player%], not a @racket[modulo-player%].  However, it's in the
the @tt{modulo-player%} file, so there's an easy fix.  


Lack of abstraction (pair0PQ):

@filebox["modulo-player%"]{
@#reader scribble/comment-reader
(racketblock
 ;; warp : Real Real -> ModuloPlayer
 ;; change the location of this player to the given location
 (define/public (warp x y)
   (new modulo-player%
	(field dest-x)
	(field dest-y)
	x y))
)
}

@filebox["player%"]{
@#reader scribble/comment-reader
(racketblock
 ;; warp : Real Real -> Player
 ;; change the location of this player to the given location
 (define/public (warp x y)
   (new player%
	(field dest-x)
	(field dest-y)
	x y))
)
}

This works correctly (this is the fix for the bug in Pair0MN's
solution), but it duplicates code.  

We want to fix these bugs without duplicating code.  

Possible solutions (suggested in class):
@itemlist[
@item{Parameterize the @racket[teleport] method with a class name.
Unfortunately, this doesn't work because the class name in
@racket[new] is not an expression.}
@item{Use @racket[this] as the class name.  This doesn't work because
@racket[this] is an @emph{instance}, not a @emph{class}.}
]

The solution is to add a new method to the interface, which constructs
a new method of the appropriate class.  So, we add this method to the
@racket[player%] class:

@racketblock[
(define/public (move x y)
  (new player% x y))
]

And this method to the @racket[modulo-player%] class:

@racketblock[
(define/public (move x y)
  (new modulo-player% x y))
]

Here's an example of the technique in full.  We start with these classes:

@codeblock{
#lang class1
(define-class s%
  (fields x y))

;; A Foo is one of:
;; - (new c% Number Number)
;; - (new d% Number Number)

(define-class c%
  (super s%)
  (define/public (make x y) (new c% x y))
  (define/public (origin) (new c% 0 0)))
(define-class d%
  (super s%)
  (define/public (make x y) (new d% x y))
  (define/public (origin) (new d% 0 0)))
}


Now we abstract the @racket[origin] method to use @racket[make], and
we can abstract @racket[origin] to the superclass @racket[s%], since
it becomes identical in both classes, avoiding the code duplication.

@codeblock{
#lang class1
(define-class s%
  (fields x y)
  (send this make 0 0))

;; A Foo is one of:
;; - (new c% Number Number)
;; - (new d% Number Number)

(define-class c%
  (super s%)
  (define/public (make x y)
    (new c% x y)))
(define-class d%
  (super s%)
  (define/public (make x y) 
    (new d% x y)))

(new c% 50 100)
(send (new c% 50 100) origin)
}



@section{Abstracting list methods with different representations}

Here is the list interface from the last homework assignment:

@#reader scribble/comment-reader
(racketblock
; ==========================================================
; Parametric lists

; A [Listof X] implements list<%>.
(define-interface list<%>
  [; X -> [Listof X]
   ; Add the given element to the front of the list.
   cons
   ; -> [Listof X]
   ; Produce the empty list.
   empty
   ; -> Nat
   ; Count the number of elements in this list.
   length
   ; [Listof X] -> [Listof X]
   ; Append the given list to the end of this list.
   append
   ; -> [Listof X]
   ; Reverse the order of elements in this list.
   reverse
   ; [X -> Y] -> [Listof Y]
   ; Construct the list of results of applying the function
   ; to elements of this list.
   map
   ; [X -> Boolean] -> [Listof X]
   ; Construct the list of elements in this list that
   ; satisfy the predicate.
   filter
   ; [X Y -> Y] Y -> Y
   ; For elements x_0...x_n, (f x_0 ... (f x_n b)).
   foldr
   ; [X Y -> Y] Y -> Y
   ; For elements x_0...x_n, (f x_n ... (f x_0 b)).
   foldl])
)

Here's the usual implementation of a small subset of this interface,
first for the recursive union implementation:

@#reader scribble/comment-reader
(racketblock
  (define-class cons%
    (fields first rest)
    
    (define/public (cons x)
      (new cons% x this))
    
    (define/public (empty)
      (new empty%))

    (define/public (length)
      (add1 (send (field rest) length)))

    (define/public (foldr c b)
      (c (field first)
	 (send (field rest) foldr c b))))

  (define-class empty%
    
    (define/public (cons x)
      (new cons% x this))
    
    (define/public (empty)
      this)

    (define/public (length)
      0)

    (define/public (foldr c b)
      b)))

And for the wrapper list implementation:

@#reader scribble/comment-reader
(racketblock
  (define-class wlist%
    (fields ls)
    
    (define/public (cons x)
      (new wlist% (ls:cons x (field ls))))
    
    (define/public (empty)
      (new wlist% ls:empty))

    (define/public (length)
      (ls:length (field ls)))

    (define/public (foldr c b)
      (ls:foldr c b (field ls))))
)

None of these look the same, so how can we abstract?  Our abstraction
design recipe for using inheritance requires that methods look
identical in order to abstract them into a common super class.  But,
for example, the @r[length] method looks like this for @r[wlist%]:

@#reader scribble/comment-reader
(racketblock
    (define/public (length)
      (ls:length (field ls))))

Like this for @r[empty%]:

@#reader scribble/comment-reader
(racketblock
    (define/public (length)
      0))

And like this for @r[cons%]:

@#reader scribble/comment-reader
(racketblock
    (define/public (length)
      (add1 (send (field rest) length))))


@margin-note{In fact, all of
		them---but
		that's a topic
		for another day.}
Before we can abstract this method, we must make them all look the
same. Fortunately, many list operations
can be expressed using just a few simple operations, of which the most
important is @r[foldr].  Here's an implementation of @r[length] which
just uses @r[foldr] and simple arithmetic.  

@#reader scribble/comment-reader
(racketblock
    (define/public (length)
      (send this foldr (λ (a b) (add1 b)) 0))
)

Note that this isn't specific to any one implementation of lists---in
fact, we can use it for any of them.  This means that we can now
abstract the method, creating a new @r[list%] class to share all of
our common code:


@#reader scribble/comment-reader
(racketblock
 (define-class list%
    (define/public (length)
      (send this foldr (λ (a b) (add1 b)) 0))
    (code:comment "other methods here"))
)

The only methods that need to be implemented differently for different
list versions are @r[empty] and @r[cons], because they construct new
lists, and @r[foldr], because it's the fundamental operation we use to
build the other operations out of.  It's also helpful to implementat
@r[foldl], since it's fairly complex to factor out.  

@section{Solidifying what we've done}

So far in this class, we've seen a number of different ways of
specifying the creation and behavior of the data we work with.  At
this point, it's valuable to take a step back and consider all of the
concepts we've seen, and how they differ from what we had in Fundies
1. 

@subsection{Data Definitions}

Data defintions describe how data is constructed.  For example,
primitive classes of data such as @tt{Number} and @tt{String} are
examples, of data defintions, as is @r[(make-posn Number Number)].  We
can also describe enumerations and unions, just as we did previously.  

In this class, we've introduced a new way of writing data defintions,
referring to @emph{classes}.  For example:

@racketblock[
(code:comment "A WList is (new wlist% [Listof X])")
]

We can combine this style of data defintion with other data definition
forms, such as unions.  However, classes also need to describe one
other important aspect---their @emph{interface}.  So we will add the
following to the above data defintion:

@racketblock[
(code:comment "A WList is (new wlist% [Listof X])")
(code:comment "    and implements the IList interface")
]

@subsection{Interface Definitions}

An @emph{interface defintion} lists the operations that something that
implements the interface will support.  

@subsection{Contracts}

@subsection{Design Recipe}


@section{Delegation}

Seperate concept from mechanism.

Class 0 without helper functions:

@#reader scribble/comment-reader
(racketmod
  class0
  ;; A BT is one of:
  ;; - (new leaf% Number)
  ;; - (new node% Number BT BT)

  ;; double : Number -> BT
  ;; Double this tree and put the number on top.

  (define-class leaf%
    (fields number)
    
    (define/public (double n)
      (new node% n this this)))

  (define-class node%
    (fields number left right)
    
    (define/public (double n)
      (new node% n this this)))
)

@#reader scribble/comment-reader
(racketblock
  (define-class helper%
    ;; Number BT -> BT
    ;; Double the given tree and puts the number on top.
    (define/public (double-helper number bt)
      (new node% number bt bt)))
  (define tutor (new helper%))

  (define-class leaf%
    (fields number)
    
    (define/public (double n)
      (send tutor double-helper n this)))

  (define-class node%
    (fields number left right)
    
    (define/public (double n)
      (send tutor double-helper n this)))
)
