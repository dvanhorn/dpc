#lang scribble/manual
@(require scribble/eval
          class/utils
          racket/sandbox
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ... check-expect))
          (for-label (except-in class/0 define-struct ... check-expect))
          (for-label class/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class/0))
    (the-eval '(require 2htdp/image))
    the-eval))

@title{Classes of Objects: Data Definitions}

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

@classblock{
;; A Posn is (new posn% Real Real)
(define-class posn%
  (fields x y))
}

Methods can compute with any given arguments and the object that
calling the method, thus the template for a @racket[posn%] method is:

@classblock{
;; posn%-method : Z ... -> ???
(define (posn%-method z ...)
  ... (send this x) (send this y) z ...)
}

Here we see that our template lists the available parts of the
@racket[posn%] object, in particular the two fields @racket[x] and
@racket[y].

@section{Enumerations}

An @deftech{enumeration} is a data definition for a finite set of
possibilities.  For example, we can represent a traffic light like the
ones on Huntington Avenue with a finite set of symbols, as we did in
Fundies I:

@classblock{
;; A Light is one of:
;; - 'Red
;; - 'Green
;; - 'Yellow
}

Following the design recipe, we can construct the template for
functions on @tt{Light}s:

@classblock{
;; light-function : Light -> ???
(define (light-function l)
  (cond [(symbol=? 'Red l) ...]
        [(symbol=? 'Green l) ...]
        [(symbol=? 'Yellow l) ...]))
}

Finally, we can define functions over @tt{Light}s, following the template.  
@classblock{
;; next : Light -> Light
;; Next light after the given light
(check-expect (next 'Green) 'Yellow)
(check-expect (next 'Red) 'Green)
(check-expect (next 'Yellow) 'Red)
(define (next l)
  (cond [(symbol=? 'Red l) 'Green]
        [(symbol=? 'Green l) 'Yellow]
        [(symbol=? 'Yellow l) 'Red]))
}

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

@classblock{
(check-expect (send ex1 count) 1)
(check-expect (send ex2 count) 5)
(check-expect (send ex3 count) 3)
}

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

@filebox[
 (racket node%)
 @classblock{
  ;; count : -> Number
  ;; count the number of numbers in this node
  (define (count)
    (send this number) ...
    (send (send this left) count) ...
    (send (send this right) count) ...)}]

Now we provide a definition of the @racket[count] method for each of
our classes.

@filebox[
 (racket leaf%)
 @classblock{
  ;; count : -> Number
  ;; count the number of numbers in this leaf
  (define (count)
    1)}]

@filebox[
 (racket node%)
 @classblock{
  ;; count : -> Number
  ;; count the number of numbers in this node
  (define (count)
    (+ 1
       (send (send this left) count)
       (send (send this right) count)))}]

Next, we want to write the @racket[double] function, which takes a
number and produces two copies of the @tt{BT} with the given number at
the top.  Here is a straightforward implementation for @racket[leaf%]:

@filebox[
 (racket leaf%)
 @classblock{
  ;; double : Number -> BT
  ;; double this leaf and put the number on top
  (define (double n)
    (new node%
         n
         (new leaf% (send this number))
         (new leaf% (send this number))))}]

Note that @racket[(new leaf% (send this number))] is just constructing a
new @racket[leaf%] object just like the one we started with.
Fortunately, we have a way of referring to ourselves, using the
identifier @racket[this].  We can thus write the method as:

@filebox[
 (racket leaf%)
 @classblock{
  ;; double : Number -> BT
  ;; double this leaf and put the number on top
  (define (double n)
    (new node% n this this))}]

For @racket[node%], the method is very similar:
@margin-note{Since these two methods are so similar, you may wonder if
they can be abstracted to avoid duplication.  We will see how to do
this in a subsequent class.}

@filebox[
 (racket node%)
 @classblock{
  ;; double : Number -> BT
  ;; double this node and put the number on top
  (define (double n)
    (new node% n this this))}]

The full @tt{BT} code is now:
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

@include-section{02/more-rocket.scrbl}
@include-section{02/exercises.scrbl}