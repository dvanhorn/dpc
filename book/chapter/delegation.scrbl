#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/1 define-struct ... length))
          (for-label 2htdp/image)
          (for-label class/universe))

@title[#:tag "chapter:delegation"]{Abstraction via Delegation}


@section{Constructor design issue in modulo zombie (Assignment 3,
Problem 3)}

Course staff solution for regular zombie game:

@filebox[@r[world%]]{
@classblock{
(define (teleport)
  (new world%
       (new player%
            (random WIDTH)
            (random HEIGHT))
       (this . zombies)
       (this . mouse)))
}}

This has a significant bug: it always produces a plain
@racket[player%], not a @racket[modulo-player%].

Bug (pair0MN):

@filebox[@r[modulo-player%]]{
@classblock{
(define (teleport)
  (new player%
       (* -1 (random WORLD-SIZE))
       (* -1 (random WORLD-SIZE))))
}}

This has a similar bug: it always produces a plain
@racket[player%], not a @racket[modulo-player%].  However, it's in the
the @tt{modulo-player%} file, so there's an easy fix.  


Lack of abstraction (pair0PQ):

@filebox[@r[modulo-player%]]{
@classblock{
;; warp : Real Real -> ModuloPlayer
;; change the location of this player to the given location
(define (warp x y)
  (new modulo-player%
       (this . dest-x)
       (this . dest-y)
       x y))
}}

@filebox[@r[player%]]{
@classblock{
;; warp : Real Real -> Player
;; change the location of this player to the given location
(define (warp x y)
  (new player%
       (this . dest-x)
       (this . dest-y)
       x y))
}}

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
a new instance of the appropriate class.  So, we add this method to the
@racket[player%] class:

@classblock{
(define (move x y)
  (new player% x y))
}

And this method to the @racket[modulo-player%] class:

@classblock{
(define (move x y)
  (new modulo-player% x y))
}

Here's an example of the technique in full.  We start with these classes:

@codeblock{
#lang class/1
(define-class s%
  (fields x y))

;; A Foo is one of:
;; - (new c% Number Number)
;; - (new d% Number Number)

(define-class c%
  (super s%)
  (define (make x y) (new c% x y))
  (define (origin) (new c% 0 0)))
(define-class d%
  (super s%)
  (define (make x y) (new d% x y))
  (define (origin) (new d% 0 0)))
}


Now we abstract the @racket[origin] method to use @racket[make], and
we can abstract @racket[origin] to the superclass @racket[s%], since
it becomes identical in both classes, avoiding the code duplication.

@codeblock{
#lang class/1
(define-class s%
  (fields x y)
  (this . make 0 0))

;; A Foo is one of:
;; - (new c% Number Number)
;; - (new d% Number Number)

(define-class c%
  (super s%)
  (define (make x y)
    (new c% x y)))
(define-class d%
  (super s%)
  (define (make x y) 
    (new d% x y)))

(new c% 50 100)
((new c% 50 100) . origin)
}



@section{Abstracting list methods with different representations}

Here is a parametric list interface definition:

@classblock{
;; ==========================================================
;; Parametric lists

;; A [Listof X] implements
;;
;; cons : X -> [Listof X]
;; Add the given element to the front of the list.
;;
;; empty : -> [Listof X]
;; Produce the empty list.
;;
;; length : -> Nat
;; Count the number of elements in this list.
;;
;; append : [Listof X] -> [Listof X]
;; Append the given list to the end of this list.
;;
;; reverse : -> [Listof X]
;; Reverse the order of elements in this list.
;;
;; map : [X -> Y] -> [Listof Y]
;; Construct the list of results of applying the function
;; to elements of this list.
;;
;; filter : [X -> Boolean] -> [Listof X]
;; Construct the list of elements in this list that
;; satisfy the predicate.
;;
;; foldr : [X Y -> Y] Y -> Y
;; For elements x_0...x_n, (f x_0 ... (f x_n b)).
;;
;; foldl : [X Y -> Y] Y -> Y
;; For elements x_0...x_n, (f x_n ... (f x_0 b)).
}

Here's the usual implementation of a small subset of this interface,
first for the recursive union implementation:

@classblock{
(define-class cons%
  (fields first rest)
    
  (define (cons x)
    (new cons% x this))
    
  (define (empty)
    (new empty%))

  (define (length)
    (add1 (this . rest . length)))

  (define (foldr c b)
    (c (this . first)
       (this . rest . foldr c b))))

(define-class empty%
    
  (define (cons x)
    (new cons% x this))
    
  (define (empty)
    this)

  (define (length)
    0)

  (define (foldr c b)
    b))
}

And for the wrapper list implementation:

@classblock{
(define-class wlist%
  (fields ls)
    
  (define (cons x)
    (new wlist% (ls:cons x (this . ls))))
    
  (define (empty)
    (new wlist% ls:empty))

  (define (length)
    (ls:length (this . ls)))

  (define (foldr c b)
    (ls:foldr c b (this . ls))))
}

None of these look the same, so how can we abstract?  Our abstraction
design recipe for using inheritance requires that methods look
identical in order to abstract them into a common super class.  But,
for example, the @r[length] method looks like this for @r[wlist%]:

@classblock{
(define (length)
  (ls:length (this . ls)))
}

Like this for @r[empty%]:

@classblock{
(define (length)
  0)
}

And like this for @r[cons%]:

@classblock{
(define (length)
  (add1 (this . rest . length)))
}


@margin-note{In fact, all of
                them---but
                that's a topic
                for another day.}
Before we can abstract this method, we must make them all look the
same. Fortunately, many list operations
can be expressed using just a few simple operations, of which the most
important is @r[foldr].  Here's an implementation of @r[length] which
just uses @r[foldr] and simple arithmetic.  

@classblock{
(define (length)
  (this . foldr (λ (a b) (add1 b)) 0))
}

Note that this isn't specific to any one implementation of lists---in
fact, we can use it for any of them.  This means that we can now
abstract the method, creating a new @r[list%] class to share all of
our common code:

@classblock{
(define-class list%
  (define (length)
    (this . foldr (λ (a b) (add1 b)) 0))
  ;; other methods here
  )
}

The only methods that need to be implemented differently for different
list versions are @r[empty] and @r[cons], because they construct new
lists, and @r[foldr], because it's the fundamental operation we use to
build the other operations out of.  It's also helpful to implementat
@r[foldl], since it's fairly complex to factor out.  

@section{Delegation}

So far, we've seen multiple ways to abstract repeated code.  First, in Fundies
1, we saw functional abstraction, where we take parts of functions that differ
and make them parameters to the abstracted function.  Second, in this class
we've seen abstraction  by using inheritance, where if methods in two related
classes are identical, they can be lifted into one method in a common
superclass.  

However, can we still abstract common code without @emph{either} of these
mechanisms?  Yes.  

Consider the @racketmodname[class/0] language, @emph{without} helper
functions.  We can write a binary tree class like this:

@codeblock{
#lang class/0
;; A BT is one of:
;; - (new leaf% Number)
;; - (new node% Number BT BT)

;; double : Number -> BT
;; Double this tree and put the number on top.

(define-class leaf%
  (fields number)
  
  (define (double n)
    (new node% n this this)))

(define-class node%
  (fields number left right)
  
  (define (double n)
    (new node% n this this)))
}

Unfortunately, the @r[double] method is identical in both the @r[leaf%] and
@r[node%] classes.  How can we abstract this without using inheritance or a
helper function?

One solution is to create a new class, and @emph{delegate} the responsibility
of doing the doubling to it.  Below is an example of this:

@classblock{
(define-class helper%
  ;; Number BT -> BT
  ;; Double the given tree and puts the number on top.
  (define (double-helper number bt)
    (new node% number bt bt)))

(define tutor (new helper%))

(define-class leaf%
  (fields number)
    
  (define (double n)
    (tutor . double-helper n this)))

(define-class node%
  (fields number left right)
    
  (define (double n)
    (tutor . double-helper n this)))
}

The @r[helper%] class has just one method, although we could add as many as we
wanted.  We also need only one instance of @r[helper%], called @r[tutor],
although we could create new instances when we needed them as well.  Now the
body of @r[double-helper] contains all of the doubling logic in our program,
which might become much larger without needing duplicate code.  
