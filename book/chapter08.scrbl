#lang scribble/manual
@(require "../web/utils.rkt"
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class1 check-expect define-struct ... length
				numerator denominator))
	  (for-label 2htdp/image)
	  (for-label (only-in test-engine/racket-tests check-expect))
	  (for-label class1/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class2))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@title[#:tag "lec09"]{Constructors}

@section{Canonical forms}

Today we're going to look more at the concept of @emph{invariants}.
Invariants often let us write code that takes advantage of the fact
that we know some property, the invariant, of our data.  We saw this
last class using sorted lists of numbers.  Today we're going to
examine a new example: fractions.

A fraction can be represented as a compound data that consists of two
numbers representing the numerator and denominator:

@codeblock{
  ;; A Fraction is a (new fraction% Integer Integer).
  (define-class fraction%
    (fields numerator denominator))
}

The problem here is that we'd like to consider the
fractions:

@codeblock{
  (new fraction% 1 2)
  (new fraction% 2 4)
}

as representing the same number, namely @racket[1/2], but these are
different representations of the same information.  The issue with
fractions is a recurring issue we've seen with information that allows
for multiple representations (sets are another example).  

There are a couple approaches to solving this issue:

@itemlist[#:style 'ordered

@item{Represents information is some @emph{canonical} way.}

@item{Codify the interpretation of data as a program.}
]

The first approach basically eliminates the problem of multiple
representations by picking a unique representation for any given piece
of information.  For example, with fractions, we might choose to
represent all fractions in lowest terms.  This means any fraction
admits only a single representation and therefore any fractions which
are interpreted as "the same" have exactly the same structure.  (This
approach is not always possible or feasible.)

The second approach requires us to write a program (a function, a
method, etc.) that determines when two pieces of data are interpreted
as representing the same information.  For example, we could write a
method that converts fractions to numbers and compares them for
numerical equality; or we simplify the fraction to lowest terms and
compare them structurally.

Along the lines of the second approach, let's consider adding the
following method:

@filebox[@r[fraction%]]{
@#reader scribble/comment-reader
(racketblock
  ;; to-number : -> Number
  ;; Convert this fraction to a number.
  (define/public (to-number)
    (/ (field numerator)
       (field denominator)))
)}

This method essentially embodies our interpretation of the
@racket[fraction%] class of data.  It doesn't help with this issues:

@codeblock{
  (check-expect (new fraction% 1 2)
		(new fraction% 2 4))
}

But of course now we can write our tests to rely on this
interpretation function:

@codeblock{
  (check-expect ((new fraction% 1 2) . to-number)
		((new fraction% 2 4) . to-number))
}

But what if we wanted to go down the second route?  We could define a
method that computes a fraction in lowest terms:

@filebox[@r[fraction%]]{
@#reader scribble/comment-reader
(racketblock
  ;; simplify : -> Fraction
  ;; Simplify a fraction to lowest terms.
  (check-expect ((new fraction% 3 6) . simplify)
		(new fraction% 1 2))
)}

We can use the @racket[gcd] function to compute the greatest common
denominator of the terms:

@filebox[@r[fraction%]]{
@#reader scribble/comment-reader
(racketblock
  (define/public (simplify)
    (new fraction%
	 (/ (field numerator) 
	    (gcd (field numerator) 
		 (field denominator)))
	 (/ (field denominator) 
	    (gcd (field numerator) 
		 (field denominator)))))
)}

This allows us to structurally compare two fractions that have been
simplified to lowest terms:

@codeblock{
  (check-expect ((new fraction% 3 6) . simplify)
		((new fraction% 1 2) . simplify))
}

But it does not prevent us from constructing fractions that are not in
lowest terms, which is what we were aiming for --- we want it to be an
invariant of @racket[fraction%] objects that they are in their
simplest form.  One possibility is to define a @emph{constructor
function} that consumes a numerator and denominator and constructs a
@racket[fraction%] object in lowest terms:

@codeblock{
;; fract-constructor : Number Number -> Fraction
;; construct a fraction in lowest terms.
(define (fract-constructor n d)
  (new fraction%
       (/ n (gcd n d))
       (/ d (gcd n d))))
}

So we are able to write a new function with the behavior we want and
it establishes our invariant.  That's good, but there are still some
inconveniences:

@itemlist[
@item{We have to write a function.}
@item{We have to remember to use it everywhere in place of the constructor.}
@item{We still have the @racket[fraction%] constructor around, which allows
users, including ourselves, to violate the invariant.}
]

If we want to have a stronger guarantee that we maintain the lowest
term invariant, we need a stronger mechanism for enforcing our
discipline at construction-time.  The idea is to allow arbitrary
computation to occur between the call to a constructor and the
initialization of an object.  To enable this mechanism, we need to
bump the language level up to @racket[class2].

All @racket[class1] programs continue to work in @racket[class2].  The
main difference is that we now the ability to write @emph{constructors}.

@filebox[@r[fraction%]]{
@codeblock{
  (constructor (n d)
    ;;...some expression that uses the fields form to return values
    ;;   for all of the fields...
    ...)
}}

The @racket[constructor] form can take any number of arguments
and must use the @racket[fields] to initialize each of the fields.
If you leave off the constructor form, a default constructor is
generated as:

@codeblock{
  (constructor (n d)
    (fields n d))
}

And in general if you have @tt{n} fields, the defaults constructor
looks like:

@codeblock{
  (constructor (field1 field2 ... fieldn)
    (fields field1 field2 ... fieldn))
}

But by writing our own constructor, we can insert computation to
convert arguments in a canonical form.  For our @racket[fraction%]
class, we can use the following code:

@codeblock{
  ;; Number Number -> Fraction
  (constructor (n d)
    (fields (/ n (gcd n d))
	    (/ d (gcd n d))))
}

This code is used every time we have a @racket[(new fraction% Number
Number)] expression.  Since this is the only way to construct a
fraction, we know that @emph{all fractions are represented in lowest
terms}.  It is simply impossible, through both error or malice, to
construct an object that does not have this property.

Returning to our @racket[simplify] method; we don't really need it any
longer.  (We could, if need be, re-write the code to take advantage of
the invariant and give a correct implementation of
@racket[simplify] as @racket[(define/public (simplify) this)], since
all fractions are already simplified.)  Likewise, we no longer need
the @racket[fract-constructor] function.

Finally, we get to the point we wanted:
@codeblock{
  (check-expect (new fraction% 1 2)
		(new fraction% 2 4))
}

Q: Can you have multiple constructor?  

A: No.  We've been thinking about multiple constructors, but we don't
have a strong story for them yet.  Remember: you can always write
functions and you can think of these as alternative constructors.

That brings up another feature in the @racketmodname[class2] language
--- constructors and functions are treated more uniformly now: you may
leave off the @racket[new] keyword when constructing objects.

@examples[#:eval the-eval
  (define-class posn%
    (fields x y))
  (new posn% 2 3)
  (posn% 4 5)]

Q: Can you have a different number of arguments to the constructor
than to the number of fields?  

A: Yes.  There's no relation between the number of arguments to your
constructor and the number of fields in the object being constructed.

One thing to note is that printing values has changed.  You'll notice
that @racket[fraction%] values no longer print as @racket[(new
fraction% Number Number)], but instead as @racket[(object:fraction%
Number Number)].  This is because by adding arbitrary computation at
construction-time, there's no longer a close relationship between a 
call to a constructor and the contents of an object.  So in printing
values we have a choice to make: either print the constructor call,
which doesn't tell us about the contents of the object, @emph{or} 
print the contents of the object, which doesn't tell us about the
call to the constructor.  We chose the latter.

Q: Can you call methods on the object being constructed?  

A: No.  What would they do?  Suppose you invoked a method that
referred to fields of @racket[this] object --- those things just
don't exist yet.

Some languages allow this.  Java for example, will let you invoke
methods from within constructors and should those methods reference
fields that are not initialized, bad things happen.  (This is just
poor language design, and descends from
@link["http://en.wikipedia.org/wiki/C._A._R._Hoare"]{Sir Tony Hoare}'s
"Billion Dollar Mistake": the null reference.)


@section{Integrity checking}

Beyond computing canonical forms, constructors are also useful for
checking the integrity of data given to a constructor.  For example,
suppose we are writing a class to represent dates in terms of their
year, month, and day of the month.  Now, what if we're given the 67th
day of March in the year -17?  What should that data represent?  Maybe
it should be March 40 (because as we heard in class, @racket[(= 40 (-
67 17))]; maybe it should be May 6th, 17 B.C., maybe it should May
6th, 17 years before the UNIX epoch of 1970; maybe it should be March
5, 17 A.D., which we arrive at by mod'ing 67 by the number of days in
March and making the year positive; or maybe... this data is just
bogus and we should raise an error and refuse to continue computing.

Let's see how we can implement a simple form of @emph{integrity checking}
in a constructor.  We will implement a class to represent dates and 
raise an error in case of a situation like the above.

@codeblock{
  ;; A Date is (date% Number Number Number).
  ;; Interp: Year Month Day.
  ;; Year must be positive.
  ;; Month must be in [1,12].
  ;; Day must be in [1,31].
  (define-class date%
    (fields year month day))
}

We can still construct meaningless dates, so what we would like to do
is check the inputs to a constructor make some sense.  This let's us
establish the integrity of all @racket[date%] objects --- if you have 
your hands on a @racket[date%] object, you can safely assume it
satisfies the specification we've given in the data definition.

The simplest way to satisfy the specification is with this constructor:

@filebox[@r[date%]]{
@codeblock{
  (constructor (y m d)
    (error "I didn't like this date!"))
}}

This is known as a "sound" solution in the program verification
community.  Notice: if you have your hands on a @racket[date%] object,
you can safely assume it satisfies the specification we've given in
the data definition.  Why?  Because you @emph{cannot} construct a
@racket[date%] object.

We'd like to do better by accepting more legitimate dates.  Here is
one that accepts all the things deemed acceptable in our specification
(this is both "sound" and "complete"):

@filebox[@r[date%]]{
@codeblock{
  (constructor (y m d)
    (cond [(<= y 0) (error "year was negative or zero")]
          [(or (> m 12) (< m 1)) (error "month too big or too small")]
          [(or (> d 31) (< d 1)) (error "day too big or too small")]
          [else (fields y m d)]))
}}

@(the-eval 
  '(define-class date%
     (fields year month day)
     (constructor (y m d)
       (cond [(<= y 0) (error "year was negative or zero")]
	     [(or (> m 12) (< m 1)) (error "month too big or too small")]
	     [(or (> d 31) (< d 1)) (error "day too big or too small")]
	     [else (fields y m d)]))))

@examples[#:eval the-eval
  (date% 2011 3 67)]

@margin-note{It is still possible to construct meaningless dates, such
as February 31, 2011.  However, more stringent validation is just some
more code away, and since we are more concerned with the
@emph{concept} of integrity checking than in a robust date library, we
won't go into the details.}

Thus we can @emph{establish} invariants with computation, or we can
@emph{reject} inputs that don't have the invariant we want to
maintain.  And we can combine these approaches.  (You may want to
compute fractions in lowest terms @emph{and} reject @racket[0] as
a denominator in @racket[fraction%], for example.)

@section{Ordered binary trees}

Now we want to look at a slightly larger program and how we use
constructors to enforce important invariants.  In this section, we want
to develop a @emph{representation of sorted lists of numbers}, which
is what we did in the @seclink["lec08"]{last lecture}, but this time
we're going to represent a sorted list of numbers as an @emph{ordered
binary tree}.

An ordered binary tree looks like this:

@verbatim{
      *
     / \
    *   3
   / \
  1   2
}

Notice that there is data only at the leaves of the tree and that if
you traverse the leaves in left-to-right order, you recover the sorted
list of numbers.  Thus there is an important invariant about this data
structure: whenever we have an ordered binary tree node, the left
sub-tree is sorted and the right sub-tree is sorted @emph{and} and
numbers in the left sub-tree are smaller than or equal to all the
numbers in the right sub-tree.

Here is our data and class definition for ordered binary trees:

@codeblock{
  ;; A OBT is one of:
  ;; - (node% OBT OBT)
  ;; - (leaf% Number)
  (define-class leaf%
    (fields number))
  (define-class node%
    (fields left right))
}

Some examples:

@codeblock{
  (leaf% 7)
  (node% (leaf% 1) (leaf% 2))
}

Now, is this an example?

@codeblock{
  (node% (leaf% 7) (leaf% 2))
}

This example points out that we are currently missing the
specification of our invariant in the data definition:

@codeblock{
  ;; A OBT is one of:
  ;; - (node% OBT OBT)
  ;; - (leaf% Number)
  ;; Invariant: numbers are in ascending order from left to right.
}

What happens if we try to construct something that violates our
invariant?  Nothing -- we just construct bogus things.  Now how
could enforce this ascending order invariant?

Well, let's first think about the @racket[leaf%] case.  We are given a
number and we need to construct an ordered binary tree, meaning all
the numbers in the tree are in ascending order.  Since we are
constructing a tree with only one number in it, it's trivial to
enforce this invariant---it's always true!

Now consider the @racket[node%] case.  We are given two ordered binary
trees.  What does that mean?  It means the numbers of each tree are in
ascending order.  But wait---isn't that the property we are trying to
enforce?  Yes.  Notice that if we @emph{assume} this of the inputs and
@emph{guarantee} this of the constructed value, then it @emph{must be
true of all @tt{OBT}s}; i.e. the assumption was valid.  If this
reasoning seems circular to you, keep in mind this is not unlike "the
magic of recursion", which is not magic at all, but seems to be since
it lets you assume the very function you are writing works in
recursive calls on structurally smaller inputs.  If you do the right
thing in the base case, and if on that assumption of the recursive
call, you can construct the correct result, then that assumption about
the recursive call was valid and your program is correct for all inputs.

OK, so the challenge at hand is not in verifying that the input
@tt{OBT}s posses the invariant, but in guaranteeing that the result of
the constructor possesses it.  If we can do that, than we know the
given @tt{OBT}s must have the property.

But now this assumption is not sufficient to guarantee that the
default constructor works:

@filebox[@r[node%]]{
@codeblock{
  ;; OBT OBT -> OBT
  (constructor (a b)
    (fields a b))
}}

Why?  Although we know that the left and right sub-tree are @tt{OBT}s,
we know nothing about the relationship @emph{between} the left and
right sub-tree, which was an important part of the invariant.  Consider
for example, the @tt{OBT}s:

@codeblock{
  (node% (leaf% 4) (leaf% 5))
  (node% (leaf% 2) (leaf% 3))
}

Independently considered, these are definitely @tt{OBT}s.  However, if
we construct a @racket[node%] out of these two trees, we get:

@codeblock{
  (node% (node% (leaf% 4) (leaf% 5))
         (node% (leaf% 2) (leaf% 3)))
}

which is definitely @emph{not} an @tt{OBT}.  (Thus we have broken the
stated contract on the constructor.)

We could correctly @emph{compute} an @tt{OBT} by determining that, in
this example, the first given tree needs to be the right sub-tree and
the second given tree needs to be the left sub-tree.  We can make such
a determination based on the maximum and minimum numbers in each of
the given trees, and that suggest the following constructor:

@filebox[@r[node%]]{
@codeblock{
  ;; OBT OBT -> OBT
  (constructor (a b)
    (cond [(<= (b . max) (a . min))
           (fields b a)]
          [(<= (a . max) (b . min))
           (fields a b)]
          [else
           ...]))
}}

The @racket[max] and @racket[min] methods are easily dismissed from
our wish list:

@filebox[@r[leaf%]]{
@codeblock{
(define/public (min)
  (field n))
(define/public (max)
  (field n))
}}

@filebox[@r[node%]]{
@codeblock{
(define/public (min)
  ((field left) . min))
(define/public (max)
  ((field right) . max))
}}

At this point, our constructor does the right thing when given two
@tt{OBT}s that do not overlap, as in the example we considered, but a
troubling pair of examples to ponder over is:

@codeblock{
  (node% (leaf% 2) (leaf% 4))
  (node% (leaf% 3) (leaf% 5))
}

Again, considered independently, these are definitely @tt{OBT}s, but
there's no way to construct an ordered binary tree with one of these
as the left and the other as the right; either order you pick will be
wrong.  This case is the @racket[else] clause of our constructor.
What should we do?  One solution is just to reject this case and raise
and error:

@filebox[@r[node%]]{
@codeblock{
  ;; OBT OBT -> OBT
  (constructor (a b)
    (cond [(<= (b . max) (a . min))
           (fields b a)]
          [(<= (a . max) (b . min))
           (fields a b)]
          [else
           (error "trees overlap")]))
}}

But really this again fails to live up to the stated contract since we
should be able to take any two @tt{OBT}s and construct an @tt{OBT} out
of them.  We know that if the trees overlap, we can't simple make a
node with them as sub-trees; we have to do something more sophisticated.
Here's an idea: insert all of the elements of one into the other.
So long as we make this insertion do the right thing, our constructor
will succeed in maintaining the invariant properly.

So if we indulge in some wishful thinking and suppose we have a
@racket[insert-tree] in our interface:

@codeblock{
  ;; insert-tree : OBT -> OBT
  ;; Insert all elements of this tree into the given one.
}

then we can write the constructor as follows:

@filebox[@r[node%]]{
@#reader scribble/comment-reader
@racketblock[
  ;; OBT OBT -> OBT
  (constructor (a b)
    (cond [(<= (b . max) (a . min))
           (fields b a)]
          [(<= (a . max) (b . min))
           (fields a b)]
          [else
           (local [(define t (a #,(racketidfont ".") insert-tree b))]
	     (fields (t . left) (t . right)))]))
]}

That leaves @racket[insert-tree] to be written.  First let's consider
the case of inserting a @racket[leaf%] into a tree.  If we again rely
on some wishful thinking and relegate the work to another method that
inserts a number into a list, we can easily write @racket[insert-tree]
for the @racket[leaf%] case:

@filebox[@r[leaf%]]{
@codeblock{
  (define/public (insert-tree other)
    (send other insert (field number)))
}}

In the @racket[node%] case, if we first consider the template (the
inventory of what we have available to use), we have:

@filebox[@r[node%]]{
@racketblock[
  (define/public (insert-tree other)
    ((field left) #,(racketidfont ".") insert-tree other) ... 
    ((field right) #,(racketidfont ".") insert-tree other) ...)
]}

But here we don't really want to insert the left tree into the other
and the right into the other.  We want to insert the right tree into
the other, then insert the left tree into @emph{that one} (other
permutations of the order of insertions would work, too).  That leads
us to:

@filebox[@r[node%]]{
@racketblock[
  (define/public (insert-tree other)
    ((field left) #,(racketidfont ".") insert-tree ((field right) #,(racketidfont ".") insert-tree other)))
]}

We have only a single item remaining on our wish list---we need to
implement the @racket[insert] method for inserting a single number
into a tree.

First let's consider the case of inserting a number into a
@racket[leaf%].  If we have a leaf and we insert a number into it, we
know we get a node with two leaves.  But where should the inserted
number go?  One solution is to compare the inserted number against the
existing number to determine which side the number should go to:


@filebox[@r[leaf%]]{
@codeblock{
(define/public (insert m)
  (node% (leaf% (the-real-min n m))
         (leaf% (the-real-max n m))))
}}

In the case of inserting a number into a node, we compare the number
against the maximum of the left sub-tree to determine if the number
should be inserted in the left or right:

@filebox[@r[node%]]{
@racketblock[
  (define/public (insert n)
    (cond [(> n ((field left) . max))
           (node% (field left)
		  ((field right) #,(racketidfont ".") insert n))]
	  [else
	   (node% ((field left) #,(racketidfont ".") insert n)
		  (field right))]))
]}

