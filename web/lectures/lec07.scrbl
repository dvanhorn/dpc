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

Here's the usual implementation of a small subset of this interface:

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
      b))

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

None of these look the same, so how can we abstract?

@#reader scribble/comment-reader
(racketblock
  (define-class list%
    (define/public (length)
      (send this foldr (λ (a b) (add1 b)) 0)))
)


@section{New language features}

Interface intheritance

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


Dot notation


@section{Constructors in interfaces}

@#reader scribble/comment-reader
(racketblock
 (define-class s%
   (fields x y)
   
   (define/public (origin)
     (send this make 0 0)))

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
)

@section{Solidifying what we've done}

DD

ID

DR


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
