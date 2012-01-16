#lang scribble/manual
@(require class/utils
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

@title{Inheritance}

@section{Method inheritance with binary trees}

We developed classes for representing binary trees and wrote a couple
methods for binary trees, but one of the troubling aspects of this
code is the fact that the two implementations of @racket[double] are
identical:
@#reader scribble/comment-reader
(racketmod
  class/0
  ;; A BT is one of:
  ;; - (new leaf% Number)
  ;; - (new node% Number BT BT)

  (define-class leaf%
    (fields number)
    ...
    ;; Number -> BT
    ;; double the leaf and put the number on top
    (define/public (double n)
      (new node% n this this)))
  
  (define-class node%
    (fields number left right)
    ...
    ;; Number -> BT
    ;; double the node and put the number on top
    (define/public (double n)
      (new node% n this this)))
  )
If we think by analogy to the structural version of this code, we have
something like this:

@#reader scribble/comment-reader
(racketmod
 class/0
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
@racketmodname[class/1], which is a superset of
@racketmodname[class/0]---all @racketmodname[class/0] programs are
@racketmodname[class/1] programs, but not vice versa.  The key
difference is the addition of the @racket[(super _class-name)] form.

@(the-eval
  '(module m class/1
     (provide bt% node% leaf%)
     (define-class bt%
       ;; -> BT
       ;; Double this tree and put the number on top.
       (define/public (double n)
	 (new node% n this this)))

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
	 1))))

@(the-eval '(require 'm))

At this point we can construct binary trees just as before, 
and all binary trees understand the @racket[double] method
even though it is only defined in @racket[bt%]:

@interaction[#:eval the-eval
(new leaf% 7)
(send (new leaf% 7) double 8)
(send (send (new leaf% 7) double 8) double 9)
]

@section{"Abstract" classes}

At this point, it is worth considering the question: what does a
@racket[bt%] value represent?  We have arrived at the @racket[bt%]
class as a means of abstracting identical methods in @racket[leaf%]
and @racket[node%], but if I say @racket[(new bt%)], as I surely can,
what does that @emph{mean}?  The answer is: @emph{nothing}.

Going back to our data definition for @tt{BT}s, it's clear that the
value @racket[(new bt%)] is @bold{not} a @tt{BT} since a @tt{BT} is
either a @racket[(new leaf% Number)] or a @racket[(new node% Number BT
BT)].  In other words, a @racket[(new bt%)] makes no more sense as a
representation of a binary tree than does @racket[(new node% "Hello Fred"
'y-is-not-a-number add1)].  With that in mind, it doesn't make
sense for our program to ever construct @racket[bt%] objects---they
exist purely as an abstraction mechanism.  Some languages, such as
Java, allow you to enforce this property by declaring a
class as "abstract"; a class that is declared abstract cannot be
constructed.  Our language will not enforce this property, much as it
does not enforce contracts.  Again we rely on data definitions to 
make sense of data, and @racket[(new bt%)] doesn't make sense.

@section{Data inheritance with binary trees}

Inheritance allows us to share methods amongst classes, but it also
possible to share data.  Just as we observed that @racket[double] was
the same in both @racket[leaf%] and @racket[node%], we can also
observe that there are data similarities between @racket[leaf%] and
@racket[node%].  In particular, both @racket[leaf%] and @racket[node%]
contain a @racket[number] field.  This field can be abstracted just
like @racket[double] was---we can lift the field to the @racket[bt%]
super class and elimate the duplicated field in the subclasses:

@#reader scribble/comment-reader
(racketblock
 (define-class bt%
   (fields number)
   ;; -> BT
   ;; Double this tree and put the number on top.
   (define/public (double n)
     (new node% n this this)))

 (define-class leaf%
   (super bt%)
   ;; -> Number
   ;; count the number of numbers in this leaf
   (define/public (count)
     1))

 (define-class node%
   (super bt%)
   (fields left right)
   ;; -> Number
   ;; count the number of numbers in this node
   (define/public (count)
     (+ 1
	(send (field left) count)
	(send (field right) count)))))


The @racket[leaf%] and @racket[node%] class now inherit both the
@racket[number] field and the @racket[double] method from
@racket[bt%].  This has a consequence for making new instances.
Previously it was straightforward to make an object: you write
down @racket[new], the class name, and as many expressions as there
are fields in the class.  But now that a class may inherit fields, you
must write down as many expressions as there are fields in the class
definition itself and in all of the super classes.  What's
more, the order of arguments is important.  The fields defined in the
class come first, followed by the fields in the immediate super class,
followed by the super class's super classes, and so on.  Hence, we
still instantiate @racket[leaf%]s as before, but the arguments to @racket[new]
for @racket[node%] are changed: it takes the left subtree, the
right subtree, and @emph{then} the number at that node:

@(the-eval
  '(module n class/1
     (provide bt% node% leaf%)
     (define-class bt%
       (fields number)
       ;; -> BT
       ;; Double this tree and put the number on top.
       (define/public (double n)
	 (new node% n this this)))

     (define-class node%
       (super bt%)
       (fields left right)
       ;; -> Number
       ;; count the number of numbers in this node
       (define/public (count)
	 (+ 1
	    (send (field left) count)
	    (send (field right) count))))

     (define-class leaf%
       (super bt%)
       ;; -> Number
       ;; count the number of numbers in this leaf
       (define/public (count)
	 1))))

@(the-eval '(require 'n))

@#reader scribble/comment-reader
(racketblock
 ;; A BT is one of:
 ;; - (new leaf% Number)
 ;; - (new node% BT BT Number)
)

@interaction[#:eval the-eval
(new leaf% 7)
(new node% (new leaf% 7) (new leaf% 13) 8)
]

Although none of our method so far have needed to access the
@racket[number] field, it is possible to access @racket[number] in
@racket[leaf%] and @racket[node%] (and @racket[bt%]) methods
@emph{as if} they had their own @racket[number] field.  Let's write a
@racket[sum] method to make it clear:

@margin-note{Notice that the @racket[double] method swapped the order
of arguments when constructing a new @racket[node%] to reflect the
fact that the node constructor now takes values for its fields first,
then values for its inherited fields.}

@#reader scribble/comment-reader
(racketblock
 (define-class bt%
   (fields number)
   ;; -> BT
   ;; Double this tree and put the number on top.
   (define/public (double n)
     (new node% this this n)))

 (define-class leaf%
   (super bt%)
   ;; -> Number
   ;; count the number of numbers in this leaf
   (define/public (count)
     1)

   ;; -> Number
   ;; sum all the numbers in this leaf
   (define/public (sum)
     (field number)))

 (define-class node%
   (super bt%)
   (fields left right)
   ;; -> Number
   ;; count the number of numbers in this node
   (define/public (count)
     (+ 1
	(send (field left) count)
	(send (field right) count)))

   ;; -> Number
   ;; sum all the numbers in this node
   (define/public (sum)
     (+ (field number)
	(send (field left) sum)
	(send (field right) sum))))
)

As you can see, both of the @racket[sum] methods refer to the
@racket[number] field, which is inherited from @racket[bt%].

@section{Inheritance with shapes}

Let's consider another example and see how data and method inheritance
manifests.  This example will raise some interesting issues for how
super classes can invoke the methods of its subclasses.  Suppose we
are writing a program that deals with shapes that have position.  To
keep the example succint, we'll consider two kinds of shapes: circles
and rectangles.  This leads us to a union data definition (and class
definitions) of the following form:

@margin-note{We are using @tt{+Real} to be really precise about the
kinds of numbers that are allowable to make for a sensible notion of a
shape.  A circle with radius @racket[-5] or @racket[3+2i] doesn't make
a whole lot of sense.}

@#reader scribble/comment-reader
(racketblock
 ;; A Shape is one of:
 ;; - (new circ% +Real Real Real)
 ;; - (new rect% +Real +Real Real Real)
 ;; A +Real is a positive, real number.
 (define-class circ%
   (fields radius x y))
 (define-class rect%
   (fields width height x y))
)

Already we can see an opportunity for data abstraction since
@racket[circ%]s and @racket[rect%]s both have @racket[x] and
@racket[y] fields.  Let's define a super class and inherit these
fields:

@#reader scribble/comment-reader
(racketblock
 ;; A Shape is one of:
 ;; - (new circ% +Real Real Real)
 ;; - (new rect% +Real +Real Real Real)
 ;; A +Real is a positive, real number.
 (define-class shape%
   (fields x y))
 (define-class circ%
   (super shape%)
   (fields radius))
 (define-class rect%
   (super shape%)
   (fields width height))
)

Now let's add a couple methods: @racket[area] will compute the area of the
shape, and @racket[draw-on] will take a scene and draw the shape on
the scene at the appropriate position:

@#reader scribble/comment-reader
(racketblock
 (define-class circ%
   (super shape%)
   (fields radius)
   
   ;; -> +Real
   (define/public (area)
     (* pi (sqr (field radius))))
   
   ;; Scene -> Scene
   ;; Draw this circle on the scene.
   (define/public (draw-on scn)
     (place-image (circle (field radius) "solid" "black")
		  (field x)
		  (field y)
		  scn)))

 (define-class rect%
   (super shape%)
   (fields width height) 
   
   ;; -> +Real
   ;; Compute the area of this rectangle.
   (define/public (area)
     (* (field width)
	(field height)))
   
   ;; Scene -> Scene
   ;; Draw this rectangle on the scene.
   (define/public (draw-on scn)
     (place-image (rectangle (field width) (field height) "solid" "black")
		  (field x)
		  (field y)
		  scn)))
)

@(the-eval
  '(module p class/1
     (provide circ% rect%)
     (require 2htdp/image)

     (define-class shape%
       (fields x y)
       
       ;; Scene -> Scene
       ;; Draw this shape on the scene.
       (define/public (draw-on scn)
	 (place-image (send this img)
		      (field x)
		      (field y)
		      scn)))

     (define-class circ%
       (super shape%)
       (fields radius)
       
       ;; -> +Real
       (define/public (area)
	 (* pi (sqr (field radius))))
       
       ;; -> Image
       ;; Render this circle as an image.
       (define/public (img)
	 (circle (field radius) "solid" "black")))

     (define-class rect%
       (super shape%)
       (fields width height) 
       
       ;; -> +Real
       ;; Compute the area of this rectangle.
       (define/public (area)
	 (* (field width)
	    (field height)))
       
       ;; -> Image
       ;; Render this rectangle as an image.
       (define/public (img)
	 (rectangle (field width) (field height) "solid" "black")))))

@(the-eval '(require 'p))

@examples[#:eval the-eval
(send (new circ% 30 75 75) area)
(send (new circ% 30 75 75) draw-on (empty-scene 150 150))
(send (new rect% 30 50 75 75) area)
(send (new rect% 30 50 75 75) draw-on (empty-scene 150 150))
]

The @racket[area] method is truly different in both variants of the
shape union, so we shouldn't attempt to abstract it by moving it to
the super class.  However, the two definitions of the @racket[draw-on]
method are largely the same.  If they were @emph{identical}, it would
be easy to abstract the method, but until the two methods are
identical, we cannot lift the definition to the super class.  One way
forward is to rewrite the methods by pulling out the parts that differ
and making them seperate methods.  What differs between these two
methods is the expression constructing the image of the shape, which
suggests defining a new method @racket[img] that constructs the image.
The @racket[draw-on] method can now call @racket[img] and rewriting it
this way makes both @racket[draw-on] methods identical; the method can
now be lifted to the super class:

@#reader scribble/comment-reader
(racketblock
 (define-class shape%
   (fields x y)

   ;; Scene -> Scene
   ;; Draw this shape on the scene.
   (define/public (draw-on scn)
     (place-image (img)
		  (field x)
		  (field y)
		  scn)))
)

But there is a problem with this code.  While this code makes sense
when it occurrs inside of @racket[rect%] and @racket[circ%], it
doesn't make sense inside of @racket[shape%].  In particular, what
does @racket[img] mean here?  The @racket[img] method is a method of
@racket[rect%] and @racket[circ%], but not of @racket[shape%],
and therefore the name @racket[img] is unbound in this context.

On the other hand, observe that all shapes are either @racket[rect%]s
or @racket[circ%]s.  We therefore @emph{know} that the object invoking
the @racket[draw-on] method understands the @racket[img] message,
since both @racket[rect%] and @racket[circ%] implement the
@racket[img] method.  Therefore we can use @racket[send] to invoke the
@racket[img] method on @racket[this] object and thanks to our data
definitions for shapes, it's guaranteed to succeed.  (The message send
would fail if @racket[this] referred to a @racket[shape%], but remember
that @racket[shape%]s don't make sense as objects in their own right
and should never be constructed).

We arrive at the folllowing final code:

@#reader scribble/comment-reader
(racketmod
 class/1
 (require 2htdp/image)

 ;; A Shape is one of:
 ;; - (new circ% +Real Real Real)
 ;; - (new rect% +Real +Real Real Real)
 ;; A +Real is a positive, real number.
 (define-class shape%
   (fields x y)
   
   ;; Scene -> Scene
   ;; Draw this shape on the scene.
   (define/public (draw-on scn)
     (place-image (send this img)
		  (field x)
		  (field y)
		  scn)))

 (define-class circ%
   (super shape%)
   (fields radius)
   
   ;; -> +Real
   ;; Compute the area of this circle.
   (define/public (area)
     (* pi (sqr (field radius))))
   
   ;; -> Image
   ;; Render this circle as an image.
   (define/public (img)
     (circle (field radius) "solid" "black")))

 (define-class rect%
   (super shape%)
   (fields width height) 
   
   ;; -> +Real
   ;; Compute the area of this rectangle.
   (define/public (area)
     (* (field width)
	(field height)))
   
   ;; -> Image
   ;; Render this rectangle as an image.
   (define/public (img)
     (rectangle (field width) (field height) "solid" "black")))

 (check-expect (send (new rect% 10 20 0 0) area)
	       200)
 (check-within (send (new circ% 10 0 0) area) 
	       (* pi 100) 
	       0.0001)
 (check-expect (send (new rect% 5 10 10 20) draw-on 
		     (empty-scene 40 40))
	       (place-image (rectangle 5 10 "solid" "black") 
			    10 20
			    (empty-scene 40 40)))
 (check-expect (send (new circ% 4 10 20) draw-on 
		     (empty-scene 40 40))
	       (place-image (circle 4 "solid" "black")
			    10 20
			    (empty-scene 40 40)))
)

@section{Exercises}

@subsection[#:tag "assign_range"]{Shapes}

Here is the signature for a method to compute the area of a 
shape's bounding box---the smallest rectangle that can contain 
the shape.
@#reader scribble/comment-reader
(racketblock
  ;; bba : -> Number     (short for "bounding-box-area")
  ;; Compute the area of the smallest bounding box for this shape.
)

Here are some examples of how @r[bba] should work:
@#reader scribble/comment-reader
(racketblock
  (check-expect ((rect% 3 4) . bba) 12)
  (check-expect ((circ% 1.5) . bba)  9)
)

@itemlist[
@item{Design the @r[bba] method for the
  @r[rect%] and @r[circ%] class.}

@item{Design a super class of @r[rect%] and @r[circ%] and lift the
  @r[bba] method to the super class.  Extend the shape interface as
  needed, but implement any methods you add.}

@item{Design a new variant of a Shape, Square, which should support
  all of the methods of the interface.}
]
