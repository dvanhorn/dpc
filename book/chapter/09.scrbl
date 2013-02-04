#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/1 check-expect define-struct ... length))
          (for-label 2htdp/image)
          (for-label (only-in test-engine/racket-tests check-expect))
          (for-label class/universe))

@title[#:tag "programinvariants"]{Invariants, Testing, and Abstraction Barriers}

@section{Invariants of Data Structures}

Here's an interface for a sorted list of numbers.

@codeblock{
#lang class/1
;; An ISorted implements
;; insert : Number -> Sorted
;; contains? : Number -> Boolean
;; ->list : -> [Listof Number]
;; empty? : -> Boolean

;; Invariant: The list is sorted in ascending order.  

;; Precondition: the list must not be empty
;; max : -> Number
;; min : -> Number
}

How would we implement this interface?  

We can simply adopt the recursive union style that we've already seen
for implementing lists.  Here we see the basic defintion as well as
the implementation of the @r[contains?] method.
@codeblock{
#lang class/1
;; A Sorted is one of
;; - (new smt%)
;; - (new scons% Number Sorted)
(define-class smt%
  (check-expect ((new smt%) . contains? 5) false)
  (define (contains? n)
    false))

(define-class scons%
  (fields first rest)
  (check-expect ((new scons% 5 (new smt%)) . contains? 5) true)
  (check-expect ((new scons% 5 (new smt%)) . contains? 7) false)
  (check-expect ((new scons% 5 (new scons% 7 (new smt%))) . contains? 3)
                false)
  (check-expect ((new scons% 5 (new scons% 7 (new smt%))) . contains? 9)
                false)
  (define (contains? n)
    (or (= n (field first))
        ((field rest) . contains? n))))
}


However, we can write a new implementation that uses our invariant to
avoid checking the rest of the list when it isn't necessary.  

@classblock{
  (define (contains? n)
    (cond [(= n (field first)) true]
          [(< n (field first)) false]
          [else ((field rest) . contains? n)]))
}

Because the list is always sorted in ascending order, if @r[n] is less
than the first element, it must be less than every other element, and
therefore can't possibly be equal to any element in the list.  

Now we can implement the remaining methods from the interface. First, @r[insert]

@filebox[@r[smt%]]{
@classblock{
(check-expect ((new smt%) . insert 5) 
              (new scons% 5 (new smt%)))
(define (insert n)
  (new scons% n (new smt%)))
}}

@filebox[@r[scons%]]{
@classblock{
(check-expect ((new scons% 5 (new smt%)) . insert 7)
              (new scons% 5 (new scons% 7 (new smt%))))
(check-expect ((new scons% 7 (new smt%)) . insert 5)
              (new scons% 5 (new scons% 7 (new smt%))))
(define (insert n)
  (cond [(< n (field first))
         (new scons% n this)]
        [else
         (new scons%
              (field first)
              ((field rest) . insert n))]))
}}

Note that we don't have to look at the whole list to insert the
elements.  This is again a benefit of programming using the
invariant that we have a sorted list.

Next, the @racket[max] method.  
We don't have to do anything for the empty list, because we have a 
precondition that we can only call @r[max] when the list is non-empty.

@filebox[@r[scons%]]{
@classblock{
(define real-max max)
(check-expect ((new scons% 5 (new smt%)) . max) 5)
(check-expect ((new scons% 5 (new scons% 7 (new smt%))) . max) 7)
(define (max)
  (cond [((field rest) . empty?) (field first)]
        [else ((field rest) . max)]))
}}

Again, this implementation relies on our data structure invariant.  To
make this work, though, we need to implement @r[empty?].

@filebox[@r[smt%]]{
@classblock{
(check-expect ((new smt%) . empty?) true)
(define (empty?) true)
}}

@filebox[@r[scons%]]{
@classblock{
(check-expect ((new scons% 1 (new smt%)) . empty?) false)
(define (empty?) false)
}}

The final two methods are similar.  Again, we don't implement @r[min]
in @r[smt%], because of the precondition in the interface.  
@filebox[@r[smt%]]{
@classblock{
;; no min method
(define (->list) empty)
}}

@filebox[@r[scons%]]{
@classblock{
(define (min) (field first))
(define (->list)
  (cons (field first) ((field rest) . ->list)))
}}


@section{Properties of Programs and  Randomized Testing}

A @emph{property} is a claim about the behavior of a program.  Unit
tests check particular, very specific properties, but often there are
more general properties that we can state and check about programs.  

Here's a property about our sorted list library, which we would like
to be true:

@tt{∀ sls : ISorted . ∀ n : Number . }@racket[((sls #,(racketidfont ".") insert n) #,(racketidfont ".") contains? n)]

How would we check this?  We can check a few instances with unit
tests, but this property makes a very strong claim.  If we were
working in ACL2, as in the Logic and Computation class, we could
provide a machine-checked proof of the property, verifying that it is
true for every single @tt{Sorted} and @tt{Number}.  

For something in between these two extremes, we can use @emph{randomized
testing}.  This allows us to gain confidence that our property is
true, with just a bit of programming effort.  

First, we want to write a program that asks the question associated
with this property.

@classblock{
;; Property: forall sorted lists and numbers, this predicate holds
;; insert-contains? : ISorted Number -> Boolean
(define (insert-contains? sls n)
  ((sls . insert n) . contains? n))
}

Now we make lots of randomly generated tests, and see if the predicate
holds. First, let's build a random sorted list generator.

@classblock{
;; build-sorted : Nat (Nat -> Number) -> Sorted
(define (build-sorted i f)
  (cond [(zero? i) (new smt%)]
        [else
         (new scons% 
              (f i)
              (build-sorted (sub1 i) f))]))
(build-sorted 5 (lambda (x) x))
}

Oh no!  We broke the invariant.  The @r[scons%] constructor allows you
to break the invariant, and now all of our methods don't work.
Fortunately, we can implement a fixed version that uses the @r[insert]
method to maintain the sorted list invariant:

@classblock{
;; build-sorted : Nat (Nat -> Number) -> Sorted
(define (build-sorted i f)
  (cond [(zero? i) (new smt%)]
        [else
         ((build-sorted (sub1 i) f) . insert (f i))]))
(check-expect (build-sorted 3 (lambda (x) x))
              (new scons% 1 (new scons% 2 (new scons% 3 (new smt%)))))
}

Now @r[build-sorted] produces the correct answer, which we can easily
verify at the Interactions window.

Using @r[build-sorted], we can develop @r[random-sorted], which
generates a sorted list of random numbers.:

@classblock{
;; Nat -> Sorted
(define (random-sorted i)
  (build-sorted i (lambda (_) (random 100))))
}

Given these building blocks,
we can write a test that checks our property.  

@classblock{
(check-expect (insert-contains? (random-sorted 30) (random 50))
              true)
}

Every time we hit the @tt{Run} button, we generate a random sorted
list of numbers, and check if a particular random integer behaves
appropriately when @r[insert]ed into it.  But if we could repeatedly
check this property hundreds or thousands of times, it would be even
more unlikely that our program violates the property.  After all, we
could have just gotten lucky.


First, we write a function to perform some action many times:
@classblock{
;; Nat (Any -> Any) -> 'done
;; run the function f i times
(define (do i f)
  (cond [(zero? i) 'done]
        [else (f (do (sub1 i) f))]))
}

Then we can run our test many times:
@classblock{
(do 1000
    (lambda (_)
      (check-expect (insert-contains? (random-sorted 30) (random 50))
                    true)))
}

When this says that we've passed 1000 tests, we'll be more sure that
we've gotten our function right.  

What if we change our property to this untrue statement?
@classblock{
;; Property: forall sorted lists and numbers, this predicate holds
;; insert-contains? : ISorted Number -> Boolean
(define (insert-contains? sls n)
  (sls . contains? n))
}

Now we get lots of test failures, but the problem is @emph{not} in our
implementation of sorted lists, it's in our property definition.  If
we had instead had a bug in our implementation, we would have
similarly seen many failures.  Thus, it isn't always possible to tell
from a test failure, or even many failures, whether it's the code or
the specification is wrong---you have to look at the test failure to
check.  

This is why it's extremely important to get your specifications (like
contracts, data definitions, and interface definitions) correct.  Your
program can only be correct if they are.

@section{Abstraction Barriers and Modules}

Recall that in our original version of @r[build-sorted], we saw that the
@r[scons%] constructor allowed us to violate the invariant---it didn't
check that the value provided for @r[first] was at least as small as
the elements of @r[rest].  We would like to prevent clients of our
sorted list implementation from having access to this capability, so
that we can be sure that our invariant is maintained.

To address this, we set up an @emph{abstraction barrier}, preventing
other people from seeing the @r[scons%] constructor.  To create these
barriers, we use a @emph{module system}.  We will consider our
implementation of sorted lists to be one module, and we can add a
simple specification to allow other modules to see parts of the
implementation (but not all of it).

Modules in our languages are very simple---you've already written
them.  They start with @tt{#lang class/N} and cover a whole file.

Here's the module implementing our sorted list, which we save in a
file called @tt{"sorted-list.rkt"}.  

@filebox[@r[sorted-list.rkt]]{
@codeblock{
#lang class/1

;; ... all of the rest of the code ...

(define smt (new smt%))
(provide smt)
}}

We've added two new pieces to this file.  First, we define @r[smt] to
be an instance of the empty sorted list.  Then, we use @r[provide] to
make @r[smt], but not @emph{any} other definition from our module,
available to other modules.  

Therefore, the @emph{only} part of our code that the rest of the world
can see is the @r[smt] value.  To add new elements, the rest of the
world has to use the @r[insert] method.  

@codeblock{
#lang class/1
(require "sorted-list.rkt")

(smt . insert 4)
}

Here, we've used @r[require], which we've used to refer to libraries
that come with DrRacket.  However, we can specify the name of a file,
and we get everything that the module in that file @r[provide]s, which
here is just the @r[smt] definition.  Everything else, such as the
dangerous @r[scons%] constructor, is hidden, and our implementation of
sorted lists can rely on its invariant.  
