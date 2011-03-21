#lang scribble/manual
@(require "../utils.rkt"
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class4 check-expect define-struct ... length
				numerator denominator))
	  (for-label 2htdp/image)
	  (for-label (only-in test-engine/racket-tests check-expect))
	  (for-label class4/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class2))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@title[#:tag "lec20"]{3/17: More about Equality}

@section[#:tag "lec20-announce"]{Announcements}

@itemlist[
@item{Exam 2 is in 5 days, 3/22, in Room 110 WVH}
@item{Next homework is out, due on @emph{Friday 3/25}.}
]



@section{Types in Java}

What sorts of things count as @emph{types} in Java? 

@itemlist[
@item{Class names}
@item{Interface names}
@item{Other stuff: @tt{int}, @tt{boolean}, ...}
]

What should be a part of the contract and purpose in Java? Well, we
don't need to write down things that are already part of the type.  If
the contract corresponds to the type, you don't have to repeat it.
However, some contracts can't be checked by the type system---you
should still write those down.

@section{Returning to the code from last time}

@verbatim|{
interface LoP {
   Boolean isEmpty();
   Posn getFirst();
   LoP getRest();
   Boolean isEqual();
}

class MT implements LoP {
   MT() {}

   public Posn getFirst() {
       return ????;
   }

   public LoP getRest() {
       return ????;
   }

   public Boolean isEmpty() {
       return true;
   }

   public Boolean isEqual(LoP lop) {
       return lop.isEmpty();
   }
}

class Cons implements LoP {
   Posn first;
   LoP rest;

   Cons(Posn first, LoP rest) {
       this.first = first;
       this.rest = rest;
   }

   public Boolean isEmpty() {
       return false;
   }

   public Boolean isEqual(LoP lop) {
       return (!lop.isEmpty())
           && this.first.isEqual(lop.getFirst())
           && this.rest.isEqual(lop.getRest());
   }

   public Posn getFirst() {
       return this.first;
   }

   public LoP getRest() {
       return this.rest;
   }


}

}|

How did we get into this situation?  Let's look at @tt{isEqual} again:

@verbatim|{
   public Boolean isEqual(LoP lop) {
       return (!lop.isEmpty())
           && this.first.isEqual(lop.getFirst())
           && this.rest.isEqual(lop.getRest());
   }
}|

After the first conjunct, we know that @tt{lop} is a @tt{Cons}, and so
we want to get the first and rest. But @emph{Java} doesn't know that,
and so we created the @tt{getFirst} and @tt{getRest} methods.

If we return @tt{null}, then we create problems for all possible
clients of the @tt{getFirst} method.  So instead we raise an error:
@;'

@verbatim|{
public Posn getFirst() {
    throw new RuntimeException("Can't take the first of an empty list.")
}

public Posn getRest() {
    throw new RuntimeException("Can't take the rest of an empty list.")
}
}|

But if we think about @tt{isEqual} again, can we help Java do
something smarter? Let's try taking @tt{getFirst} and @tt{getRest} out
of the interface.  If we do that, we get an error message about not
finding those methods in the interface. But can we persuade Java that
@tt{lop} is really a @tt{Cons}?  Yes:


@verbatim|{
   public Boolean isEqual(LoP lop) {
       return (!lop.isEmpty())
           && this.first.isEqual(((Cons)lop).getFirst())
           && this.rest.isEqual(((Cons)lop).getRest());
   }
}|

This is called @emph{casting}, and it's crucial for (some kinds of)
programming using the Java type system.  However, it's important to
remember that we're cheating the type system here.  The cast is turned
into a runtime check, and we could write anything down that we want.
It's only when running the program that Java can check whether
@tt{lop} is really a @tt{Cons}.  

@section{Equality and Parameterized Types}

We try writing a parameterized version of Listof<X> with equality.

It doesn't work, because X doesn't have an @tt{isEqual} method.  

Fix: restrict X to implement the IEqual interface.  

Question about structural vs nominal typing.

Discussion about polymorphism/bounds/extends/etc.

The class language version:

@codeblock{
;; An [IEqual X] implements
;; =? : X -> Boolean

;; A [Listof X] implements
;; empty?: -> Boolean
;; and implements [IEqual [Listof X]]
;; where X implements [IEqual X]
}


If we unroll our definitions a little, we get:

@codeblock{
;; An [IEqual X] implements
;; =? : X -> Boolean

;; A [Listof X] implements
;; empty? : -> Boolean
;; =? : [Listof X] -> Boolean
;; where X implements
;; =? : X -> Boolean
}

@section{Comparing different kinds of things}

This works:

@racketblock[
(equal? 5 "fred")
]

This doesn't work:

@tt{new Posn(3,4).isEqual("fred")}

The reason this doesn't work is that we're violating the contract on
@tt{isEqual}, which is enforced by the type system. 

So let's create a different isEqual method:

@verbatim|{
   public Boolean isEqualPosn(Posn p) {
       return this.x == p.x
           && this.y == p.y;
   }
   
   public Boolean isEqual(Object o) {
       return (o instanceof Posn)
           && this.x == p.x
           && this.y == p.y;
   }
}|

This doesn't typecheck, so we have to add casts again.

@verbatim|{
   public Boolean isEqual(Object o) {
       return (o instanceof Posn)
           && this.x == ((Posn)p).x
           && this.y == ((Posn)p).y;
   }
}|

Or, we can use our way of comparing @tt{Posn}s.

@verbatim|{
   public Boolean isEqual(Object o) {
       return (o instanceof Posn)
           && this.isEqualPosn((Posn)o);
   }
}|

But why would we want to compare things of different types?  It turns
out that Java has a built-in notion of equality, called the
@tt{equals} method.  And this method expects its input to be an
@tt{Object}.  

We can reuse this implementation:

@verbatim|{
   public Boolean equals(Object o) {
       return (o instanceof Posn)
           && this.isEqualPosn((Posn)o);
   }
}|

There are two big problems with this:

@itemlist[
@item{If we extend Posn, then equals will behave differently in
different directions.}
@item{We forgot to override @tt{hashCode}.}
]

@subsection{equals and hashCode}

The fundamental rules for overriding @tt{equal}:

@itemlist[
@item{If you override equals, you @bold{must} override @tt{hashCode}.}
@item{@tt{o.equals(p)} implies @tt{o.hashCode() == p.hashCode()}}
]

The rule of @tt{hashCode}: if two objects have differnet hash codes,
they are different.  @tt{hashCode} is a fast way to check if two
objects are different.

Here's a quick and correct implementation of @tt{hashCode}:

@verbatim|{
   public int hashCode() {
       return 0;
   }
}|
