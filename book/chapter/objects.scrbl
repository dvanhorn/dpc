#lang scribble/manual
@(require scribble/eval
          class/utils
          (for-label (except-in class/0 check-expect))
          (for-label (only-in lang/htdp-intermediate-lambda check-expect))
          (for-label class/universe))

@title[#:tag "Object_=_Data_+_Function"]{Objects = Data + Function}

One of the key concepts behind so-called @deftech{object-oriented
programming} (OOP) is the notion of an @deftech{object}.  An object is
a new kind of value that can, as a first cut, be understood as a
pairing together of two familiar concepts: @as-index{data} and
@as-index{function}.

@itemlist[

@item{@bold{An object is like a structure} in that it has a fixed
number of fields, thus an object (again, like a structure) can
represent @as-index{compound data}.  But unlike a
@as-index{structure}, an object contains not just data, but
@emph{functionality} too;}

@item{@bold{An object is like a (set of) function(s)} in
that it has behavior---it @emph{computes}; it is not just inert
data.}]

This suggests that objects are a natural fit for well-designed
programs since good programs are organized around data definitions and
@as-index{functions} that operate over such data.  An object, in
essence, packages these two things together into a single programming
apparatus.  This has two important consequences:

@itemlist[#:style 'ordered

@item{@bold{You already know how to design programs oriented around
objects.}

Since objects are just the combination of two familiar concepts that
you already use to design programs, you already know how to design
programs around objects, even if you never heard the term ``object''
before.  In short, the better you are at programming with functions,
the better you will be at programming with objects.}

@item{@bold{Objects enable new kinds of @as-index{abstraction} and
@as-index{composition}.}

Although the combination of data and function may seem simple, objects
enable new forms of abstraction and composition.  That is, objects
open up new approaches to the construction of computations.  By
studying these new approaches, we can distill new design principles.
Because we understand objects are just the combination of data and
function, we can understand how all of these principles apply in the
familiar context of programming with functions.  In short, the better
you are at programming with objects, the better you will be at
programming with functions.}]

In this chapter, we will explore the basic concepts of objects by
revisiting a familiar program, first organized around data and
functions and then again organized around objects.

@include-section{01/functional-rocket.scrbl}
@include-section{01/object-rocket.scrbl}
@include-section{01/history.scrbl}
@include-section{01/exercises.scrbl}