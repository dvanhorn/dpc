#lang scribble/manual
@(require class/utils
          scriblib/footnote
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/1 define-struct ... length))
          (for-label 2htdp/image)
          (for-label class/universe))

@title{Solidifying what we've done}

So far, we've seen a number of different ways of specifying the
creation and behavior of the data we work with.  At this point, it's
valuable to take a step back and consider all of the concepts we've
seen.

@section{Data Definitions}

A data definition defines a @emph{class}@note{Here we mean
``class'' in the general sense of ``a set of values,'' not to be
confused with the concept of a ``class'' as definied by the
@racket[define-class] form.} of values by describing how instances of
that data are constructed.  In this book, we focus on particular new
kind of value: @emph{objects}, and data definitions will primarily
define a class of objects.  New kinds of objects are made with
@racket[define-class], while instances of a class are made with
a class constructor, written @racket[(new _class-name% _arg ...)].

Data definitions can be built out of primitive data, such as
@tt{Number}s, @tt{String}s, @tt{Image}s, etc., but also compound data
can be represented with objects containing data.  Data definitions can
also be formed as the (possibly recursive) union of other data
definitions.  For example:

@classblock{
;; A ListofImage is one of:
;; - (new empty%)
;; - (new cons% Image ListofImage)
}

Data definitions may be parameterized, meaning a family of similar
data definitions is simultaneously defined by use of variable
parameters that range over other classes of values.  For example:

@classblock{
;; A [Pair X Y] is one of:
;; - (new pair% X Y)
}

Here the @tt{Pair} family of data definition is parameterized over
classes of values @tt{X} and @tt{Y}.

@section{Interface Definitions}

Another way to define a class of values is by way of an
@emph{interface definition}.  Unlike a data definition, which focuses
on how data is represented, an interface defines a set of values by
the operations that set of values support.  Interfaces provide a means
for defining a set of values independent of representation.

For example:
@codeblock[#:keep-lang-line? #f]{
#lang racket
;; A [List X] implements

;; - empty : -> [List X]
;;   Produce an empty list
;; - cons : X -> [List X]
;;   Produce a list with the given element at the front.
;; - empty? : -> Boolean
;;   Determine if this list is empty.
;; - length : -> Number
;;   Count the elements in this list
;; ... and other methods ...
}

There are several important aspects of this interface defintion to
note.  First, it lists all of the methods that can be used on an
@tt{[List X]}, along with their contracts and purpose statements.
Mere method names are not enough---with just a method name you have no
idea how to use a method, or what to use it for.  Second, interface
defintions can have parameters (here @tt{X}), just like data
defintions.  Third, there is no description of how to @emph{construct}
an @tt{[List X]}.  That's the job of data defintions that implement
this interface.

Of course, just like data defintions don't have to be named, interface
defintions don't have to be named either.  If you need to describe an interface
just once, it's fine to write the interface right there where you need it.  

@section{Contracts}

Contracts describe the appropriate inputs and outputs of functions and
methods.  In the past, we've seen many contracts that refer to data
defintions.  In this class, we've also seen contracts that refer to interface
defintions, like so:

@racketblock[(code:comment "[IList Number] -> [IList Number]")]

When describing the contract of a function or method, it's almost always
preferable to refer to an interface definition instead of a data defintion that
commits to a specific representation.  @; Sometimes, there's only one data
@; definition that makes sense, and then there isn't a difference between using
@; the interface defintion and the data defintion.  

@section{Design Recipe}

Interfaces change the design recipe in one important way.  In the Template
step, we take an inventory of what is available in the body of a function or
method.  When designing a method, we have the following available to us:
@itemlist[
@item{The fields of this object, accessed with accessor methods,}
@item{The methods of this object, accessed by calling them,}
@item{And the operations of the arguments, which are given by their @emph{interfaces}.}
]

For example, if a method takes an input @r[a-list] which is specified in the
contract to be an @tt{IList}, then we know that @r[(send a-list empty?)],
@r[(send a-list length)], and so on.  

