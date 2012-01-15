#lang scribble/manual
@(require scribble/eval
	  class/utils
          (for-label (except-in class/0 check-expect))
	  (for-label (only-in lang/htdp-intermediate-lambda check-expect))
	  (for-label class/universe))

@title{Objects = Data + Function}

One of the key concepts behind so-called @emph{object-oriented
programming} (OOP) is the notion of an @emph{object}.  An object is a
new kind of value that can, as a first cut, be understood as a pairing
together of two familiar concepts: data and function.  

@itemlist[
@item{@bold{An object is like a structure} in that it has a fixed
number of fields, thus an object (again, like a structure) can
represent compound data.  But unlike a structure, an object contains
not just data, but @emph{functionality} too;}

@item{@bold{An object is like a (set of) function(s)} in that it has
behavior---it @emph{computes}; it is not just inert data.}]

This suggests that objects are a natural fit for well-designed
programs since good programs are organized around data definitions and
functions that operate over such data.  An object, in essence,
packages these two things together into a single programming
apparatus.  This has two important consequences:

@itemlist[#:style 'ordered

@item{@bold{You already know how to design programs oriented around
objects.}

Since objects are just the combination of two familiar concepts that
you already use to design programs, you already know how to design
programs around objects, even if you never heard the term "object"
before.  In short, the better you are at programming with functions,
the better you will be at programming with objects.}

@item{@bold{Objects enable new kinds of abstraction and composition.}

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

@include-section{sec-functional-rocket.scrbl}
@include-section{sec-object-rocket.scrbl}
@;include-section{sec-more-rocket.scrbl}

@section{A Brief History of Objects}

Objects are an old programming concept that first appeared in the late
1950s and early 1960s just across the Charles river at MIT in the AI
group that was developing Lisp.  Simula 67, a language developed in
Norway as a successor to Simula I, introduced the notion of classes.
In the 1970s, Smalltalk was developed at Xerox PARC by Alan Kay and
others.  Smalltalk and Lisp and their descendants have influenced each
other ever since.  Object-oriented programming became one of the
predominant programming styles in the 1990s.  This coincided with the
rise of graphical user interfaces (GUIs), which objects model well.
The use of object and classes to organize interactive, graphical
programs continues today with libraries such as the Cocoa framework
for Mac OS X.

@include-section{sec-01-exercises.scrbl}