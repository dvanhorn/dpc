#lang scribble/manual
@(require "../utils.rkt"
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

@title[#:tag "lec17"]{3/7: Java}

@section{EWD's rules for science}

Van Horn presented some slides.

@section{Two Ideas: Java and Types}

Types are a @emph{mechanism} for enforcing data definitions and contracts.

Java is a programming language like the one we've seen, with a
different syntax and with types.

@section{Programming in Java}

@subsection{Java Syntax}

@racketblock[
(code:comment "A C is (C Number String String)")
(define-class C
  (fields x y z)
  (code:comment "Number Number -> C")
  (define/public (m p q)
    ...))
]

@verbatim|{
class C {
    Number x;
    String y;
    String z;
    public C m(Number p, Number q) {
	...
    }
    public C(Number x, String y, String z) {
	this.x = x;
	this.y = y;
	this.z = z;
    }
}
}|

Let's create a simple pair of numbers:
@;'

@verbatim|{
class Pair {
    Number left;
    Number right;
    
    public Pair(Number left, Number right) {
	this.left = left;
	this.right = right;
    }
    
    public Pair swap() {
	return new Pair(this.right, this.left);
    }
}
}|

    But really, this doesn't work, because Java doesn't have the type @tt{Number}.  So we'll choose @tt{Integer} instead.


@;'

@verbatim|{
class Pair {
    Integer left;
    Integer right;
    
    public Pair(Integer left, Integer right) {
	this.left = left;
	this.right = right;
    }
    
    public Pair swap() {
	return new Pair(this.right, this.left);
    }
}
}|

									       
To test this, we'll import a testing library:

@verbatim{
import tester.Tester;
}

and write some examples:

@;'

@verbatim|{
class Examples {
    public Examples() {}
    public boolean testSwap(Tester t) {
	return t.checkExpect(new Pair(3,4).swap(), 
			     new Pair(4,3));
    }
}
}|


@section{Running Java Programs}

We don't have a Run button in Java, so we need a different way to run
our program. To do this, we first need to install our test library.
This requires installing a @tt{JAR} file from the NU Tester
@link["http://code.google.com/p/nutester/"]{web site}.  

The we have to compile the program.  

@itemlist[
	@item{javac}
	@item{The classpath and the -cp option}
	@item{Now we have class files, which are binary and all mashed
	up}
	@item{To run this, we use the @tt{java} command, which also
	has a @tt{-cp} option}
	@item{If we change things, we have to recompile and then rerun.}
]

@section{A More Complex Example}

@;'

What if we want to represent a union?

@verbatim|{
    class Square {
	Integer size;
	public Square(Integer size) {
	    this.size = size;
	}
    }
    class Circ {
	Integer radius;
	public Circ(Integer radius) {
	    this.radius = radius;
	}
    }
}|

How do we declare that both of these are @tt{Shape}s?


@verbatim|{
    import tester.Tester;

    interface IShape {}

    class Square implements IShape {
	Integer size;
	public Square(Integer size) {
	    this.size = size;
	}
	public IShape nothing(IShape i) {
	    return i;
	}
    }

    class Circ implements IShape {
	Integer radius;
	public Circ(Integer radius) {
	    this.radius = radius;
	}
    }

    class Examples {
	Examples () {}
	public boolean testNothing(Tester t) {
	    Square s = new Square(5); // A local binding
	    return t.checkExpect(s.nothing(new Circ(2)), 
				 new Circ(2));
	}
    }
	
}|

@section{Recursive Unions}

@verbatim|{
    import tester.Tester;
    class Mt implements IList {
	public Mt() {}
    }

    class Cons implements IList {
	Integer first;
	IList rest;
	
	public Cons(Integer first, IList rest) {
	    this.first = first;
	    this.rest = rest;
	}
    }

    interface IList {}
    
    class Examples {
	public Examples() {}
	
	public boolean testList(Tester t) {
	    return t.checkExpect(new Mt(), new Mt())
		&& t.checkExpect(new Cons(5, new Mt()), new Cons(5, new Mt()));
	}
    }	

}|

@section[#:tag "java-enum"]{Enumerations}

In Fundies I, we might have written:

@codeblock{
    ;; A Title is one of
    ;; - 'dr
    ;; - 'mr
    ;; - 'ms
}
@;'

In Java, we write:

@verbatim|{
	interface ITitle {}
	class Dr implements ITitle {
	    Dr() {}
	}
	class Mr implements ITitle {
	    Mr() {}
	}
	class Ms implements ITitle {
	    Ms() {}
	}
}|

Why write these silly constructors?

@verbatim|{
	interface ITitle {}
	class Dr implements ITitle {
	    Dr() {}
	}
	class Mr implements ITitle {
	    Mr() {}
	}
	class Ms implements ITitle {
	    Integer x;
	    Ms() {}

	    public Integer m() {
		return x+1;
	    }
	}

	class Main {
	    public static void main(String[] args) {
		new Ms().m();
		return;
	    }
	}
}|


Now we get a @tt{NullPointerException}.  But what is that?

A discussion of the evils of @tt{null}.

For example, this compiles:

@verbatim|{
	interface ITitle {}
	class Dr implements ITitle {
	    Dr() {}
	}
	class Mr implements ITitle {
	    Mr() {}
	}
	class Ms implements ITitle {
	    Integer x;
	    Ms(Integer x) {
	        this.x = x;
	    }

	    public Integer m() {
		return null;
	    }
	}

	class Main {
	    public static void main(String[] args) {
		new Ms().m();
		return;
	    }
	}
}|

Never write @tt{null} in your program!

A long sermon on @tt{null}.

@section{Parameterized Data Definitions}

Consider our @tt{Pair} class:

@verbatim|{
class Pair {
    Integer left;
    Integer right;
    
    public Pair(Integer left, Integer right) {
	this.left = left;
	this.right = right;
    }
    
    public Pair swap() {
	return new Pair(this.right, this.left);
    }
}
}|

Now if we want a @tt{Pair} of @tt{String}s:

@verbatim|{
class PairString {
    String left;
    String right;
    
    public Pair(String left, String right) {
	this.left = left;
	this.right = right;
    }
    
    public Pair swap() {
	return new Pair(this.right, this.left);
    }
}
}|


This is obviously bad---we had to copy and paste.  So let's abstract:

@verbatim|{
class Pair<T,V> {
    T left;
    V right;
    
    public Pair(T left, V right) {
	this.left = left;
	this.right = right;
    }
    
    public Pair<V,T> swap() {
	return new Pair<V,T>(this.right, this.left);
    }
}

class Examples {
    public Examples() {}
    public boolean testSwap(Tester t) {
	return t.checkExpect(new Pair<Integer,Integer>(3,4).swap(), 
			     new Pair<Integer,Integer>(4,3));
    }
}

}|

@section{Abstraction}

@;'

@verbatim|{

class C {
    Integer x;
    Integer y;
    C(Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }
    
    public Integer sq() {
	return this.x * this.x;
    }
}


class D {
    Integer x;
    String z;
    D(Integer x, String z) {
	this.x = x;
	this.z = z;
    }
    
    public Integer sq() {
	return this.x * this.x;
    }
}

}|


    Now to abstract:

@verbatim|{

class S {
    Integer x;
    public Integer sq() {
	return this.x * this.x;
    }
    S(Integer x) {
	this.x = x;
    }
}

class C extends S {
    Integer y;
    C(Integer x, Integer y) {
	super(x);
	this.y = y;
    }
}


class D {
    Integer x;
    String z;
    D(Integer x, String z) {
	this.x = x;
	this.z = z;
    }
    
    public Integer sq() {
	return this.x * this.x;
    }
}

}|
