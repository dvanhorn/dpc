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

@classblock{
;; [IList Number] -> [IList Number]
}

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


@section{Design Choices}

Q: Which is considered a better design: a union with two
variants, or a single variant with a Boolean field that indicates
"which part of the union this data belongs to"?  For example, is it
better to have a @racket[live-zombie%] and @racket[dead-zombie%] class
or a single @racket[zombie%] class with a @racket[dead?] field.

A: One of the themes of this and last semester is that @emph{once you
have settled on choice for the representation of information in your
program, the structure of your program follows the structure of that
data}.  We've trained you to systematically derive code structure from
data structure; there's a recipe---you don't even have to think.
That's great because it frees up significant quantities of the most
precious and limited resource in computing: your brain.  But
unfortunately, that recipe kicks in only after you've chosen how
information will be represented, i.e. after you've written down data
definitions.  Much of the hard work in writing programs, and where
your creative energy and brain power is really needed, is going from
information to representation.  There is no recipe for this part of
program design.  You need to develop the ability to analyze problems,
take an account of what knowledge needs to be represented to solve a
problem, and practice making decisions about how that knowledge can be
represented computationally.  The good news is you don't have to be
struck by divine inspiration to manage this step.  Program design is a
process of iterative refinement: make some choices, follow the recipe.
You will discover ways to improve your initial choices, so go back and
revise, then carry out the changes in code structure those design
decision entail.  Rinse and repeat.

This is a long winded way of saying: there is no universal "right"
answer to this question.  It will depend on the larger context.  That
said, there are some important things to take into account for this
particular question.  It is much easier to add new variants to a union
than it is to create Boolean values other than @racket[true] and
@racket[false].  Good program design is often based on anticipating
future requirements.  If you think it's possible that there might be
some other kind of zombie down the road, the union design will be less
painful to extend and maintain.

@section[#:tag "Exercises (Ch. Solidify)"]{Exercises}

@subsection{JSON, Jr.}

JSON (JavaScript Object Notation) is a lightweight data-interchange
format. It is easy for humans to read and write; it is based on a
subset of the JavaScript Programming Language.  There are a large
number of huge corpora of data available on the internets.  In order
to write programs that can make use of this data, you'll need to
design a data representation of JSON values.

A JSON value can take on the following forms:@note{This definition
is drawn directly from @url{http://json.org/}}

@itemlist[
@item{An @emph{object} is an unordered set of name/value pairs.}

@item{An @emph{array} is an ordered collection of values.}

@item{A @emph{value} can be a string, or a number, or true or false or
null, or an object or an array. These structures can be nested.}]

This definition is written in the terminology of JSON, so don't
confuse a JSON object with an object in the @tt{class} language.
Likewise, don't confuse JSON strings with @tt{class} strings.

To start things off simply, let's focus on a subset of JSON we'll call
``JSON, Jr.'' which consists solely of strings and arrays.  Using the
notation of JSON, the following are examples of JSON, Jr. values:

@itemlist[
@item{@tt{"this is JSON, Jr."}}
@item{@tt{[ "this is JSON", "Jr." ]}}
@item{@tt{[ [], [ "So", "is" ], "this" ]}}]

The first example is just a string. The second is an array of two JSON
elements, which are both strings. The third is another array, but of
three elements: the first is an array of zero elements, the second is
an array of two strings, and the third is a string.

@itemlist[#:style 'ordered

@item{Design an object-based data representation for JSON, Jr values.}

@item{Design a method for counting the number of strings in a JSON,
Jr. value.}
]

@subsection{JSON}

Revise the program you developed in the previous problem to handle all
of JSON.

Design the following methods for JSON values:

@itemlist[
@item{A method for counting the number of numbers in a JSON value.}

@item{A method for summing all the numbers in a JSON value.}

@item{A method for finding the length of the longest array in a JSON value.}

@item{A method for computing the nesting depth of a JSON value.}
]

Design the following methods for JSON objects:

@itemlist[
@item{A method that works on JSON object values that takes a string
and produces the JSON value associated with that string in the object,
or @racket[false] if no such value exists.}

@item{A method that counts the number of name/value pairs in an object.}

@item{A method that extends an object by adding a given name/value pair to an object.}

@item{A method that restricts an object by subtracting a given
name/value pair from an object.}
]

Design the following methods for JSON arrays:
@itemlist[
@item{A method of computing the length of the array.}
@item{A method for indexing the @emph{i}th element of an array.}
@item{A method for reversing an array.}
]

Design the following functions for randomly generating JSON values:
@itemlist[

@item{Given a nesting depth, compute a random JSON value of at most
that nesting depth.  (It must be the case that if called repeatedly,
eventual this function will produce a JSON value of @emph{exactly} the
given nesting depth, but it may not always produce a value nested so
deep.)}

]

Design an alternative data representation of JSON values that uses a
subset of S-Expressions to model JSON values, which we'll call JSEN
(JSON S-Expression Notation).

Design a function for converting from a JSEN representation to the
object representation of that value.  Design a method for JSON values
that produces their JSEN representation.



