#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/1 check-expect define-struct ... length
                                numerator denominator apply first rest cons?))
          (for-label 2htdp/image)
          (for-label (only-in test-engine/racket-tests check-expect))
          (for-label class/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class/2))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@title[#:tag "chapter:equality"]{Extensional Equality}

One of the most useful notions of equality is that of
@deftech{extensional equality}, which means that two values are equal
if and only if one can be substituted for the other in all possible
contexts without affecting the meaning of a program.  So for example
@racket[5] is extensionally equal to @racket[5], even though in some
sense there are two different @racket[5]s: the one written first and
one written second.  Likewise, if we have the following data
representation of Cartesian points:

@classblock{
;; A Posn is a (new posn% Number Number)
(define-class posn%
  (fields x y))
}

then @racket[(new posn% 3 4)] is extensionally equal to @racket[(new
posn% 3 4)].  If we wanted to implement a method for positions that
determined if this position was equal to a given one, it would be
straightforward:
@filebox[@racket[posn%]]{
@classblock{
;; Is this posn the same as that posn?
;; =? : Posn -> Boolean
(define (=? that)
  (and (= (this . x) (that . x))
       (= (this . y) (that . y))))
}}

For some kinds of values such as numbers, strings, and positions, it's
easy to determine if two values of that kind are equal.  But it's not
possible to compare all kinds of values for extensional equality.  For
example, two functions are extensionally equal when for all equal
inputs they produce equal outputs.  Suppose we have two @tt{Number ->
Number} functions, @racket[f] and @racket[g]; how can determine if
@racket[f] is substitutable for @racket[g]?  We'll have to apply
@racket[f] to @emph{all} possible numbers @racket[n] and then ask if
@racket[(= (f n) (g n))].  But there are an infinite number of
numbers, so we'll have to apply @racket[f] and @racket[g] an infinite
number of times before being sure that @racket[f] and @racket[g] are
the same.

By exactly the same reasoning, it follows that some kinds of objects
cannot be compared for extensional equality since comparing two
@tt{[Fun Number Number]} objects involves checking @code[#:lang
"class/1"]{(= (f . apply n) (g . apply n))} for all numbers
@racket[n].

@section{Structural equality for recursive unions}

It's fairly simple to define an equality method for simple compound
structures such as @racket[posn%], however matters are complicated
when dealing with the equality of a recursive union data
representation.  For example, suppose we define lists of positions:

@classblock{
;; A ListPosn is one of
;; - (new mt%)
;; - (new cons% Posn ListPosn)
;; and implements
;; - =? : ListPosn -> Boolean
;; - Is this list the same as given list?
(define-class mt%)
(define-class cons%
  (fields first rest))
}

To see the problem consider implementing @racket[=?] in the
@racket[mt%] class:

@filebox[@racket[mt%]]{
@classblock{
;; Is this empty list the same as given list?
(define (=? that) ...)
}}

While it's clear that when evaluating the ellided @racket[...] code
that @racket[this] is a @racket[mt%] object, it not at all clear what
kind of object @racket[that].  All that is known is @racket[that] is a
@tt{ListPosn}.  Unfortunately the answer the method should produce is
entirely dependent on the @emph{kind} of object @racket[that] is.  If
@racket[that] is a @racket[mt%], the answer is @racket[true];
otherwise it is @racket[false].

At this point we have a couple options for how to proceed.  One is to
add to the @tt{ListPosn} interface methods that tell compute whether
an object is a @tt{mt%} object.  The solution, then is simple:

@filebox[@racket[mt%]]{
@classblock{
;; Is this empty list the same as given list?
;; ListPosn -> Boolean
(define (=? that)
  (cond [(that . mt?) true]
        [(that . cons?) false]))
}}

We could do likewise for the case of a @racket[cons%]:

@filebox[@racket[cons%]]{
@classblock{
;; Is this empty list the same as given list?
;; ListPosn -> Boolean
(define (=? that)
  (cond [(that . mt?) false]
        [(that . cons?)
         (and (this . first . =? (that . first))
              (this . rest . =? (that . rest)))]))
}}

Notice here that we first check that the given list is a cons, then
and only then do we access the first and second part of the cons to
determine if they are the same.  In the case of the first, we use the
@racket[posn%] @racket[=?] method to determine if the two positions
are equal; for the rest, we use the @tt{ListPosn} @racket[=?] method
to determine if the rest of the lists are equal recursively.

This approach is simple, but leaks some details about the
representation of lists could cause difficulties if we tried to extend
the set of implementations of @tt{ListPosn}.  That's because we are
mixing our style of programming; while we are using the class dispatch
mechanism to carry out case analysis on @racket[this], we are using a
type-predicate and an explicit @racket[cond] to analyze the cases of
@racket[that].  To employ a consistent object-oriented style, we
should use the dispatch mechanism to analyze @racket[that] and
eliminate the need for the @racket[cond] expressions in this program.

Here's an alternative that makes use of a common pattern for writing
methods that follow the ``cross product of inputs'' recipe, such as
@racket[=?].  First, observe that it's easy to write object specific
equality methods.  In other words, writing a method to determine if
two empty lists are equal is easy; so is writing a method to determine
if two cons lists are equal is easy:

@filebox[@racket[mt%]]{
@classblock{
;; Is this empty list the same as given empty?
;; Empty -> Boolean
(define (=-empty? that) true)
}}

@filebox[@racket[cons%]]{
@classblock{
;; Is this cons list the same as given cons?
;; Cons -> Boolean
(define (=-cons? that)
  (and (this . first . =? (that . first))
       (this . rest . =? (that . rest))))
}}

Currently these methods are variant specific---we can't send the
@racket[=-empty?] to a @racket[cons%] object, but that's an easy
restriction to lift: we simply need to lift @racket[=-empty?] and
@racket[=-cons?] to the @tt{ListPosn} interface and then implement
@racket[=-cons?] in the @racket[mt%] class and @racket[=-empty?] in
the @racket[cons%] class:

@classblock{
;; A ListPosn ... and implements:
;; - =-empty? : Empty -> Boolean
;;   Is this list the same as given empty list?
;; - =-cons? : Cons -> Boolean
;;   Is this list the same as given cons list?
}

@filebox[@racket[mt%]]{
@classblock{
;; =-cons? : Cons -> Boolean
;; Is this empty list the same as given cons list?
(define (=-cons? that) false)
}}

@filebox[@racket[cons%]]{
@classblock{
;; =-empty? : Empty -> Boolean
;; Is this cons list the same as given empty list?
(define (=-empty? that) false)
}}

Since an empty lists and a cons lists are never the same, the
implementation of both of these methods is just @racket[false].

We are now in a position to revisit our definitions of @racket[=?] for
@racket[cons%] and @racket[mt%].  Let's reconsider @racket[=?] in the
@racket[mt%] class:
@filebox[@racket[cons%]]{
@classblock{
;; =? : ListPosn -> Boolean
;; Is this empty list the same as given list?
(define (=? that) ...)
}}

If we take an inventory of what is available in @racket[...], we have
@itemlist[
@item{@racket[this] - a @racket[mt%] (and therefore, a @tt{ListPosn}),}
@item{@racket[that] - a @tt{ListPosn},}
@item{@code[#:lang "class/1"]{(this . =-empty? Empty)} - a Boolean,}
@item{@code[#:lang "class/1"]{(this . =-cons? Cons)} - a Boolean (which is always @racket[false]),}
@item{@code[#:lang "class/1"]{(that . =-empty? Empty)} - a Boolean,}
@item{@code[#:lang "class/1"]{(that . =-cons? Cons)} - a Boolean.}
]

Now, let's consider how we could put each of these methods to use.
@itemlist[

@item{@code[#:lang "class/1"]{(this . =-empty? Empty)}

We need to give an @tt{Empty} argument, but there is only one thing
that is definitely an @tt{Empty} and that is @racket[this].  However,
@code[#:lang "class/1"]{(this . =-empty? this)} doesn't compute
anything useful---it's always @racket[true].}

@item{@code[#:lang "class/1"]{(this . =-cons? Cons)}

This doesn't compute anything useful, it's always @racket[false].
(Not to mention we have nothing that is sure to be a @tt{Cons} to
supply as an argument.)}

@item{@code[#:lang "class/1"]{(that . =-empty? Empty)}

We need to give an @tt{Empty} argument and the only thing we know to
be @tt{Empty} is @racket[this].  What does @code[#:lang
"class/1"]{(that . =-empty? this)} compute?}

@item{@code[#:lang "class/1"]{(that . =-cons? Cons)}

We need to give a @tt{Cons} argument and there's nothing we know to be
a @tt{Cons} so we can't call this method safely.}
]

Of all these possibilities, only one is possible or computes anything
interesting and that's @code[#:lang "class/1"]{(that . =-empty?
Empty)}.  So what does it do?  Since it's a method invoked on
@racket[that], the object system will dispach to the appropriate
method based on what kind of object @racket[that] is.  If it's a
@racket[cons%], we're going to invoke the @racket[=-empty?] method
that always returns false, but since @racket[that] is a @racket[cons%]
and @racket[this] is a @racket[mt%], that is the desired result.  On
the other hand, if @racket[that] is a @racket[mt%], the
@racket[=-empty?] method that always returns @racket[true] is invoked.
But in that case, both @racket[this] and @racket[that] are
@racket[mt%], so again we've produced the desired result.  By similar
reasoning, we can define the correct implementation of @racket[=?] for
@racket[cons%].  Putting it all together, we get:

@classblock{
(define-class mt%
  (define (=? that) (that . =-empty? this))
  (define (=-empty? that) true)
  (define (=-cons? that) false))

(define-class cons%
  (fields first rest)
  (define (=? that) (that . =-cons? this))
  (define (=-empty? that) false)
  (define (=-cons? that)
    (and (this . first . =? (that . first))
         (this . rest . =? (that . rest)))))
}

Notice that this correctly defines equality for lists of positions and
achieves our goal of programming in an interface-oriented style that
has no explicit @racket[cond] expressions performing case analysis on
the kind of an object.

This technique which is generally applicable to methods of a recursive
union that take arguments of that same recursive union and that needs
to consider all possible scenarios for what @racket[this] and
@racket[that] could be.  The technique is often called
@deftech{double-dispatch} since it does dispatch twice, once on the
receiver and once on the argument, in order to achieve a complete case
analysis.

This technique also gives rise to a completely mechanical, i.e. no
thought involved, approach to implementing structural equality for
recursive unions.  If you have a union @tt{U} defined as the union of
variants @tt{V1}, @tt{V2}, ..., @tt{Vn}, then the approach dictates
@classblock{
;; A U ... implements
;; =? : U -> Boolean
;; =-V1? : V1 -> Boolean
;; =-V2? : V2 -> Boolean
;; ...
;; =-Vn? : Vn -> Boolean
}

Where for each @tt{Vi}, you write:

@filebox[@racket[Vi]]{
@classblock{
;; =? : U -> Boolean
(define (=? that) (that . =-Vi? this))

;; =-Vi? : Vi -> Boolean
;; Is this Vi same as that Vi?
(define (=-Vi? that)
  ;; structural recur on each component,
  ;; equal if all parts are equal.
  ...)

;; One method for each Vj where j≠i.
;; Is this Vi same as that Vj?  No.
(define (=-Vj? that) false)
...
}}

While this approach is mechanical, it's involves writing a lot of
methods (can you construct a formulae for determining the number of
method definitions you need if there are @emph{i} variants in a
union?) and can quickly become unwieldy.  Luckily we can combat the
problem throug the use of inheritence and overriding.

@section{Abstracting equality with double dispatch}

If you have a union with @emph{i} variants, each variant will need to
define @emph{i-1} @racket[=-Vj?] methods for each @emph{j≠i}, and each
of these methods will produce @racket[false].  Instead of defining all
of these methods in the variant, we can lift all @emph{i}
@racket[=-Vi?]  methods to an abstract class and in each case, return
@racket[false].  That behavior is @emph{close} to the right thing, but
not totally correct.  In particular, for each of the @racket[Vi], the
method specific to that variant, @racket[=-Vi?] will be wrong since we
want to structurally recur, not just return @racket[false] blindly.
To fix things up is easy though: simply override the @racket[=-Vi?]
method in the @racket[Vi] implementation.

To make an example concrete, here is how to abstract the structural
equality method of @tt{ListPosn} using this approach:

@classblock{
(define-class alist%
  (define (=-empty? that) false)
  (define (=-cons? that) false))

(define-class mt%
  (super alist%)
  (define (=? that) (that . =-empty? this))
  (define (=-empty? that) true))

(define-class cons%
  (super alist%)
  (fields first rest)
  (define (=? that) (that . =-cons? this))
  (define (=-cons? that)
    (and (this . first . =? (that . first))
         (this . rest . =? (that . rest)))))
}

Because there are only two variants for a list, the overall size of
the code didn't shrink (in fact it grew slightly longer), however the
approach pays off when more variants are invovled. Each class now only
defines behavior specific to that kind of object.  If we had 100
variants, adding a 101th invaraint would only involving adding one
method to @racket[alist%] and two methods to the 101th variant.
That's it.  (Compare this to what's required if you can't use
inheritance and overriding.)  Importantly, such a design is
extensible: to add new variants we have to modify the abstract class
and the new variant; we don't have to edit the one hundred existing
implementations.

@section{Equality over interpretation}

It is often the case that two peices of data are considered equal if
and only if they are structurally equal, i.e. the data is comprised of
the same thing.  However, that is not the case when there are multiple
representations of the same information.  For example, consider the
following data definition for a @deftech{set of numbers}, represented
as a list of potentially non-unique elements:

@classblock{
;; A [SetNumber X] is one of:
;; - (new mt-set%)
;; - (new cons-set% Number SetNumber)
;; Interp: as the unordered, unique elements of this list.
}

So for example, the following two sets represents the same information
but are structurally different:
@classblock{
(new cons-set% 1 (new cons-set% 2 (new mt-set%)))
(new cons-set% 2 (new cons-set% 1 (new mt-set%)))
}

So if we had instead used @racket[cons%] and @racket[mt%], the
@racket[=?] method would produce @racket[false] for these objects.

How can we define an equality method for sets that respects the
interpretation of sets so that two sets are equal if and only if they
represent the same information?

Let's start by defining set equality the way a mathematician would,
which is that two sets are equal if one is a subset of the other and
@emph{vice versa}.  This definition can be lifted to an abstract class
for sets:

@classblock{
(define-class aset%
  (define (=? that)
    (and (this . ⊆ that)
         (that . ⊆ this))))
}

Now we need to define @racket[⊆]:

@filebox[@racket[mt-set%]]{
@classblock{
;; ⊆ : SetNumber -> Boolean
;; Is this empty set a subset of that?
;; The empty set is a subset of all sets.
(define (⊆ that) true)
}}

@filebox[@racket[cons-set%]]{
@classblock{
;; ⊆ : SetNumber -> Boolean
;; Is this non-empty set a subset of that?
;; A non-empty set is a subset if that contains the first element
;; and the rest is a subset of that.
(define (⊆ that)
  (and (that . ∋ (this . first))
       (this . rest . ⊆ that)))
}}

Finally, all that remains is @racket[∋], pronounced ``contains'':

@filebox[@racket[mt-set%]]{
@classblock{
;; ∋ : Number -> Boolean
;; Does this empty set contain given number?
(define (∋ n) false)
}}

@filebox[@racket[cons-set%]]{
@classblock{
;; ∋ : Number -> Boolean
;; Does this non-empty set contain given number?
(define (∋ n)
  (or (= n (this . first))
      (this . rest . ∋ n)))
}}

@section{Detatching objects from interpretation}

In the previous section, we defined a @racket[=?] method for sets that
respected the set intpretation of objects.  Since the set and list
interpretation of equality is different, we needed different classes
of objects to represent lists and sets.  But fundamentally, we chose
the same representation (namely lists) and our methods interpreted
this representation differently.  Is it possible to instead have a
single representation that could be interpreted both as a list or a
set?  If we are to accomplish this, it cannot be through the use of
@racket[=?] method, since there can only be one @racket[=?] method per
object.  Instead what we would like to do is define, external to the
list classes, a notion of equivalence:

@classblock{
;; An [Equiv X] implements
;; - apply X X -> Boolean
;;   Are given Xs equal?
}

Moreover, an @tt{[Equiv X]} needs to be reflexive, transitive, and
symmetric, i.e. if @racket[≡] is an @tt{[Equiv X]}, it must be the
case that:

@itemlist[

@item{@code[#:lang "class/1"]{(≡ . apply x x)} is
@racket[true].}

@item{@code[#:lang "class/1"]{(≡ . apply x y)} and @code[#:lang
"class/1"]{(≡ . apply y z)} implies @code[#:lang "class/1"]{(≡ . apply
x z)}}

@item{@code[#:lang "class/1"]{(≡ . apply x y)} implies @code[#:lang "class/1"]{(≡ . apply y x)},}
]

For example, here is the implementation of the usual equivalence
relation on numbers:

@classblock{
;; A (new eqv-number%) implements [Equiv Number].
(define-class eqv-number%
  (define (apply n m) (= n m)))
}

Here is another equivalence relation, but it equates @racket[5] with
@racket[10]:

@classblock{
;; A (new eqv-number-mod-5%) implements [Equiv Number].
(define-class eqv-number-mod-5%
  (define (apply n m) (= (modulo n 5) (modulo m 5))))
}



@section{Multiple Representations}

@codeblock{
;; A Posn implements IPosn

;; An IPosn implements
;; x : -> Number
;; y : -> Number
  ;; =? : IPosn -> Bool
  ;; is the given posn the same as this one?

;; A Posn2 is a (posn2% Number Number)
(define-class posn2%
  (fields ls)
  (constructor (x y)
               (fields (list x y)))
  (define (x) (first (field ls)))
  (define (y) (second (field ls)))
  (define (=? p)
    (and (= (x) (send p x))
         (= (y) (send p y)))))
}

Now we can compare the two different kinds of posns and everything
works properly.

What about multiple representations for lists of posns?


@codeblock{
#lang class/3

;; A Posn implements IPosn.

;; An IPosn implements:
;;
;; x : -> Number
;; y : -> Number
;; =? : IPosn -> Boolean

;; A Posn2 is a (posn2% Number Number).
;; implement IPosn.
(define-class posn2%
 (fields ls)
 (constructor (x0 y0)
   (fields (list x0 y0)))

 (define (x)
   (first (field ls)))

 (define (y)
   (second (field ls)))

 (check-expect (send (posn2% 3 4) =? (posn2% 3 4)) true)
 (check-expect (send (posn2% 3 4) =? (posn2% 3 5)) false)
 (define (=? p)
   (and (= (send this x) (send p x))
        (= (send this y) (send p y)))))


(check-expect (send (posn% 1 2) =? (posn2% 1 2)) true)
(check-expect (send (posn% 1 2) =? (posn2% 1 1)) false)


;; A Posn1 is a (posn% Number Number).
;; implements IPosn.
(define-class posn%
 (fields x y)


 ;; Posn -> Boolean
 ;; Is the given posn the same as this?
 (check-expect (send (posn% 3 4) =? (posn% 3 4)) true)
 (check-expect (send (posn% 3 4) =? (posn% 3 5)) false)
 (define (=? p)
   (and (equal? (field x) (send p x))
        (equal? (field y) (send p y)))))

;; A LoP is one of:
;; - (mt%)
;; - (cons% Posn LoP)
;; implements ILoP

;; An ILoP implements:
;;
;; first : -> Posn
;; rest : -> ILoP
;; empty? : -> Boolean

(define the-real-empty? empty?)
(define the-real-first first)
(define the-real-rest rest)

(define-class list%
 (fields ls)
 (define (empty?)
   (the-real-empty? (field ls)))

 (define (first)
   (the-real-first (field ls)))

 (define (rest)
   (list% (the-real-rest (field ls))))

 (define (=? lop)
   (cond [(send this empty?) (send lop empty?)]
         [else
          (and (not (send lop empty?))
               (send (send this first) =? (send lop first))
               (send (send this rest) =? (send lop rest)))])))








;; A [Listof X] is one of:
;; - (mt%)
;; - (cons% X [Listof X])
;; where X implements
;; =? : X -> Boolean

(define-class mt%

 ;; [Listof X] -> Boolean
 ;; Is the given list of posns the same as this?
 (define (=? lop)
   (send lop empty?))

 (define (empty?)
   true))

(define-class cons%
 (fields first rest)

 (define (=? lop)
   (and (not (send lop empty?))
        (send (field first) =? (send lop first))
        (send (field rest) =? (send lop rest))))

 (define (empty?)
   false))

(check-expect (send (mt%) =? (mt%)) true)
(check-expect (send (cons% (posn% 3 4) (mt%)) =? (cons% (posn% 3 4) (mt%)))
             true)
(check-expect (send (cons% (posn% 3 4) (mt%)) =?  (mt%))
             false)
(check-expect (send (cons% (posn% 3 4) (mt%)) =? (cons% (posn% 3 5) (mt%)))
             false)

(check-expect (send (list% empty) =? (list% empty)) true)
(check-expect (send (list% (list (posn% 3 4))) =? (list% (list (posn% 3 4))))
             true)
(check-expect (send (list% (list (posn% 3 4))) =? (list% (list)))
             false)
(check-expect (send (list% (list (posn% 3 4))) =? (list% (list (posn% 3 5))))
             false)

(check-expect (send (list% (list (posn% 3 4))) =? (cons% (posn2% 3 4) (mt%)))
             true)

}

Now we can make all of the appropriate combinations work together:
different kinds of lists with the same kind of posns, the same kind of
lists with different kinds of posns, and different kinds of lists with
different kinds of posns.


@section{Parameterized Data Defintions and Equality}

Generalizing @tt{LoP} to @tt{[Listof X]}.


@codeblock{
;; A [Listof X] is one of:
;; - (mt%)
;; - (cons% X [Listof X])
}

But we have to change one more thing.  Our @r[=?] method assumes that
@r[X] implements an @r[=?] method themselves.

@codeblock{
;; A [Listof X] is one of:
;; - (mt%)
;; - (cons% X [Listof X])
;; where X implements
;; =? : X -> Boolean
}

We could also change the signature of @r[=?] to take a comparison.
But then we'd have to change all of our code.  We've lifted a
restriction, but only to things that can be compared for equality.  



@section{Exercises}

@subsection{Extensional equality JSON}

Revisit your solution to the JSON representation problem
(@secref{JSON}) and develop a method for determining if a given JSON
value is extensionally equal to another.  Note that this method must
respect the set interpretation of JSON objects.
