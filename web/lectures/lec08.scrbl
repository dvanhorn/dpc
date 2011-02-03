#lang scribble/manual
@(require "../utils.rkt"
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class1 check-expect define-struct ... length))
	  (for-label 2htdp/image)
	  (for-label (only-in test-engine/racket-tests check-expect))
	  (for-label class1/universe))

@title[#:tag "lec08"]{2/3: Abstraction, Invariants, Testing}


@section[#:tag-prefix "lec08"]{Announcements}

@itemlist[#:style 'ordered
@item{New homework is up.  It's hard.  Think about your data definition design.}
]


@section{New Language Features}

@subsection{Dot Notation}

Works in Interactions window.

@subsection{@r[check-expect] in new places}

Works inside functions, run when it's called.

Works inside class defintions, lifted out of the class.

Question: how does a @r[check-expect] work with fields when it's
inside a class?   

@codeblock{
#lang class1
(define-class c%
  (fields x y z)
  
   ;; works
  (check-expect ((new c% 1 2 3) . m) 1)
  ;; doesn't work
  (check-expect ((new c% 1 2 3) . m) (field x))
  (define/public (m) 1))

(define (f x)
  (check-expect 1 1))
(f 5)
}

What does @r[check-expect] produce?  Nothing.

Can @r[check-expect]s in classes refer to private method?  No.  And
private methods don't exist.

@section{Invariants of Data Structures}

Here's the interface for a sorted list of numbers.

@codeblock{
#lang class1
;; An ISorted implements
;; insert : Nmber -> Sorted
;; contains? : Number -> Boolean
;; ->list : -> [Listof Number]
;; empty? : -> Boolean

;; Invariant: The list is sorted in ascending order.  

;; Precondition: the list must not be empty
;; max : -> Number
;; min : -> Number
}

How do we implement this interface?  

Suggestion 1:
@codeblock{
#lang class1
;; A Sorted is a (new list% Number Sorted)
(define-class list%
  (fields first rest))
}
This doesn't work: no base case.

Suggestion 2:
@codeblock{
#lang class1
;; A Sorted is one of
;; - (new smt%)
;; - (new scons% Number Sorted)
(define-class smt%
  (check-expect ((new smt%) . contains? 5) false)
  (define/public (contains? n)
    false)
  )
(define-class scons%
  (fields first rest)
  (check-expect ((new scons% 5 (new smt%)) . contains? 5) true)
  (check-expect ((new scons% 5 (new smt%)) . contains? 7) false)
  (check-expect ((new scons% 5 (new scons% 7 (new smt%))) . contains? 3)
		false)
  (check-expect ((new scons% 5 (new scons% 7 (new smt%))) . contains? 9)
		false)
  (define/public (contains? n)
    (or (= n (field first))
	((field rest) . contains? n)))
  )
}


New implementation using the invariant:
@racketblock[
  (define/public (contains? n)
    (cond [(= n (field first)) true]
	  [(< n (field first)) false]
	  [else ((field rest) #,(racketidfont ".") contains? n)]))
]

Insert for empty lists:
@racketblock[
(check-expect ((new smt%) #,(racketidfont ".") insert 5) 
	      (new scons% 5 (new smt%)))
(define/public (insert n)
  (new scons% n (new smt%)))
]

Insert for cons lists:
@racketblock[
(check-expect ((new scons% 5 (new smt%)) #,(racketidfont ".") insert 7)
	      (new scons% 5 (new scons% 7 (new smt%))))
(check-expect ((new scons% 7 (new smt%)) #,(racketidfont ".") insert 5)
	      (new scons% 5 (new scons% 7 (new smt%))))
(define/public (insert n)
  (cond [(< n (field first))
	 (new scons% n this)]
	[else
	 (new scons%
	      (field first)
	      ((field rest) #,(racketidfont ".") insert n))]))
]

Note that we don't have to look at the whole list to insert the
elements.  This is a benefit of programming using the @emph{invariant}
that we have a sorted list.  

@racket[max]

We don't have to do anything for the empty list, because we have a 
precondition that we can only call @r[max] when the list is non-empty.

@racketblock[
(define real-max max)
(check-expect ((new scons% 5 (new smt%)) #,(racketidfont ".") max) 5)
(check-expect ((new scons% 5 (new scons% 7 (new smt%))) #,(racketidfont ".") max) 7)
(define/public (max)
  (cond [((field rest) #,(racketidfont ".") empty?) (field first)]
	[else ((field rest) #,(racketidfont ".") max)]))
]

Again, this implementation relies on our data structure invariant.  To
make this work, though, we need to implement @r[empty?].

@filebox["smt%"]{
@racketblock[
(check-expect ((new smt%) #,(racketidfont ".") empty?) true)
(define/public (empty?) true)
]}

@filebox["scons%"]{
@racketblock[
(check-expect ((new scons% 1 (new smt%)) #,(racketidfont ".") empty?) false)
(define/public (empty?) false)
]}

Question: does @r[max] work on non-natural numbers?  Answer: Yes.

The rest of the interface

@filebox["smt%"]{
@racketblock[
(code:comment "no min method")
(define/public (->list) empty)
]
}

@filebox["scons%"]{
@racketblock[
(define/public (min) (field first))
(define/public (->list)
  (cons (field first) ((field rest) #,(racketidfont ".") ->list)))
]}


@section{Properties of Programs and  Randomized Testing}

A property is a claim about the behavior of a program.  

Here's a property about our sorted list library:

@tt{∀ sls . ∀ n . }@racket[((sls #,(racketidfont ".") insert n) #,(racketidfont ".") contains? n)]

How would we check this?  We can check a few instances with unit
tests, but this property makes a very strong claim.  If we were
working in ACL2, as in the Logic and Computation class, we could
provide a machine-checked proof of the property.  

For something in between these two, we can use randomized testing.  

First, we want to write a program that asks the question associated
with this property.

@racketblock[
;; Property: forall sorted lists and numbers, this predicate holds
;; insert-contains? : ISorted Number -> Boolean
(define (insert-contains? sls n)
  ((sls #,(racketidfont ".") insert n) #,(racketidfont ".") contains? n))
]

Now we make lots of randomly generated tests, and see if the predicate
holds. First, let's build a random sorted list generator.

@racketblock[
;; build-sorted : Nat (Nat -> Number) -> Sorted
(define (build-sorted)
  (cond [(zero? i) (new smt%)]
	[else
	 (new scons% 
	      (f i)
	      (build-sorted (sub1 i) f))]))
(build-sorted 5 (lambda (x) x))
]

Oh no!  We broke the invariant.  The @r[scons%] constructor allows you
to break the invariant, and now all of our methods don't work.

@racketblock[
;; build-sorted : Nat (Nat -> Number) -> Sorted
(define (build-sorted)
  (cond [(zero? i) (new smt%)]
	[else
	 ((build-sorted (sub1 i) f) #,(racketidfont ".") insert (f i))]))
(check-expect (build-sorted 3 (lambda (x) x))
	      (new scons% 1 (new scons% 2 (new scons% 3 (new smt%)))))

]

Now it produces the correct answer, which we can test.

Now we build a random sorted list:

@racketblock[
(code:comment "Nat -> Sorted")
(define (random-sorted i)
  (build-sorted i (lambda (_) (random 100))))
]

Now we can write tests that check our property.  

@racketblock[
(check-expect (insert-contains? (random-sorted 30) (random 50))
	      true)
]

If we could repeatedly check this property, it would be even more
unlikely to allow us to make this mistake.

@racketblock[
;; Nat (Any -> Any) -> 'done
;; run the function f i times
(define (do i f)
  (cond [(zero? i) 'done]
	[else (f (do (sub1 i) f))]))
(do 1000
    (lambda (_)
      (check-expect (insert-contains? (random-sorted 30) (random 50))
		    true)))
]

What if we change our property to:
@racketblock[
;; Property: forall sorted lists and numbers, this predicate holds
;; insert-contains? : ISorted Number -> Boolean
(define (insert-contains? sls n)
  (sls #,(racketidfont ".") contains? n))
]

Now we get lots of test failures, but the problem isn't in our code,
it's in our property.  If we change our code to add a bug, we again
see lots of failures.  Thus, you can't always tell from a test failure
whether it's the code or the specification (here the
@r[insert-contains?] function) is wrong---you have to look at the test
failure to check.  

@section{Abstraction Barriers and Modules}

Recall that in our original version of @r[build-sorted], we saw that the
@r[scons%] constructor allowed us to violate the invariant.  How can
we prevent incorrect use of the constructor?

To fix this, we set up an abstraction barrier, preventing other people
from seeing the @r[scons%] class.  To create these barriers, we use a
@emph{module system}.  We will consider our implementation of sorted
lists to be one module, and we can add a simple specification to allow
other modules to see parts of the implementation (but not all of it).  

@filebox["sorted-list.rkt"]{
@codeblock{
#lang class1

;; ... all of the rest of the code ...

(define smt (new smt%))
(provide smt)
}}

Therefore, the @emph{only} part of our code that the rest of the world
can see is the @r[smt] value.  To add new elements, the rest of the
world has to use the @r[insert] method.  

@racketmod[
class1
(require "sorted-list.rkt")

(smt #,(racketidfont ".") insert 4)
]
