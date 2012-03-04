#lang scribble/manual
@(require class/utils
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

Data defintions describe how data is constructed.  For example,
primitive classes of data such as @tt{Number} and @tt{String} are
examples, of data defintions, as is @r[(make-posn Number Number)].  We
can also describe enumerations and unions, just as we did previously.  

In this class, we've introduced a new way of writing data defintions,
referring to @emph{classes}.  For example:

@classblock{
;; A [List X] is one of:
;; - (new empty%)
;; - (new cons% X [List X])
}

We can combine this style of data defintion with other data definition
forms, such as unions.  However, classes also need to describe one
other important aspect---their @emph{interface}.  So we will add the
following to the above data defintion:

@classblock{
;; A (new empty%) implements [List X]
;; A (new cons% X [List X]) implements [List X]
}

@section{Interface Definitions}

An @emph{interface defintion} lists the operations that something that
implements the interface will support.  Just as we have a convention that data
defintions start with a capital letter, interface defintions start with a
capital letter "I".  The interface defintion for @tt{[IList X]} is:

@codeblock[#:keep-lang-line? #f]{
#lang racket
;; An [IList X] implements

;; empty : -> [IList X]
;; Produce an empty list
;; cons : X -> [IList X]
;; Produce a list with the given element at the front.
;; empty? : -> Boolean
;; Determine if this list is empty.
;; length : -> Number
;; Count the elements in this list

;; ... and other methods ...
}

There are several important aspects of this interface defintion to note.
First, it lists all of the methods that can be used on an @tt{[IList X]}, along
with their contracts and purpose statements.  Mere method names are not
enough---with just a method name you have no idea how to use a method, or what
to use it for.  Second, interface defintions can have parameters (here @tt{X}),
just like data defintions.  Third, there is no description of how to
@emph{construct} an @tt{[IList X]}.  That's the job of data defintions that
implement this interface.  

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
@item{The fields of this object, accessed with @r[field],}
@item{The methods of this object, accessed by calling them,}
@item{And the operations of the arguments, which are given by their @emph{interfaces}.}
]

For example, if a method takes an input @r[a-list] which is specified in the
contract to be an @tt{IList}, then we know that @r[(send a-list empty?)],
@r[(send a-list length)], and so on.  

