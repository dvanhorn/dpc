#lang scribble/manual
@(require "../utils.rkt"
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class0 define-struct ...))
	  (for-label class0/universe))

@title[#:tag "lec04"]{1/20: Interfaces and inheritance}


@section[#:tag-prefix "lec04"]{Announcements}
@itemlist[
@item{Assignment 2 was due last night.}

@item{Assignment 3 will be out later today and is due Wednesday night.}

@item{A new programming language has been released.  Be sure to
download and install the latest @tt{.plt} file.}

@item{Are the books in?}]

@section{Discussion of assignment 2}

@subsection{Zombie}

On the subject of the zombie game, we got a bunch of emails like this:

@nested[#:style 'inset]{ My partner and I have come upon a dilemma in
Homework 2 in Honors Fundies 2 because we have not learned inheritance
yet.  We can have two separate classes Player and Zombie and tie them
together in a union called Being, but that'll result in some
significant copy-and-paste code.  The alternative is to make one class
called Being and in the *single* function that differs between players
and zombies, use a @racket[cond] based on a field (@racket['zombie] or
@racket['player]).  We were told not to do this, but we were also told
copy-and-pasting code is bad...hence the dilemma.}

and:

@nested[#:style 'inset]{ My partner and I have been working through
the current assignment and with our representation of zombies and
players there is a lot of reused code.  I was wondering if there is
some form of inheritance in the @racketmodname[class0] language that
has not been covered.  I think that inheritance would make our current
code much cleaner by solving the problem of reused code.  Is there a
way to implement inheritance in the @racketmodname[class0] language
currently?}


One of the subjects of today's lecture is inheritance, however let's
step back and ask ourselves if this dilemma is really as inescapable
as described.


@subsection{Lists}

On the subject of the list finger exercises, we got email like this:

@nested[#:style 'inset]{ We are not allowed to use class0 lists; are we
allowed to use @racket[null], or must we create our own version of
that too? }

and:

@nested[#:style 'inset]{
Should my functions also apply for empty lists (i.e. have empty lists
be represented as objects), or could we simply use the built-in
@racket[empty] datatype (on which none of my methods would work)?}

@section{Interfaces}

@subsection{Lights, redux}

@itemlist[
 @item{a look back at light}
 @item{add a draw method}
 @item{make it a world program that cycles through the lights}
 @item{what does it mean to be a light?  next and draw}
 @item{alternative design of light class}
 @item{Q: what changes in world?  A: nothing.}
 @item{motivates the use of interfaces}
 @item{add interface to light design.  both alternatives implement it,
  and the world will work with anything that implements it}]

@section{Inheritance}

@itemlist[
 @item{posn% and shapes:
  data inheritance: a rect% is a posn% + width + height,
                    a circ% is a posn% + radius}
 @item{implement some methods: area, draw-on}

 @item{a look back at binary trees: there was code duplication.
  method inheritance: adding bt%, lifting double method}

 @item{back to shapes: can we lift anything?  No, because the definition of
  draw-on is different.  *But* we can refactor by adding an img method
  that constructs the shape image.  Now draw-on is identical and
  can be lifted.

  There is a subtlety here with method invocation.  In the first
  identical implementations of draw-on, you have calls to (img), but
  when you lift, you want (send this img) because the super class
  does not have an img method. }]

