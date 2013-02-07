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

@section[#:tag "Invariant Exercises"]{Exercises}

@subsection{Quick Lists}

Van Horn has always been underwhelmed by the fact that
@racket[list-ref] is such a slow operation when you're accessing
elements deep down in a big list.  Why should it take a million
@racket[rest]s just to get the millionth element?

To combat this drawback of an otherwise lovely data structure, the
list, Van Horn has devised an idea for a new implementation of lists
that would let you get the millionth element in about 20 operations.
If his idea works, the @racket[list-ref] operation will take roughly
@emph{log(i)} steps to get the @emph{i}th element.  The other list
operations, on the other hand, would remain more or less just as
efficient as before; taking the rest of a list, for example, might
take a few more steps to compute, but it would be some small constant
number of extra steps.  In the end, we'd have something that behaves
just like a list, but with a much better @racket[list-ref] operation.

Your task is to take Van Horn's idea and implement it.  Since you'll
be building a new kind of list data structure, let's first agree on
the list interface we want:

@codeblock{
;; A [List X] implements
;; - cons : X -> [List X]
;;   Cons given element on to this list
;; - first : -> X
;;   Get the first element of this list
;;   (only defined on non-empty lists)
;; - rest : -> [List X]
;;   Get the rest of this
;;   (only defined on non-empty lists)
;; - list-ref : Natural -> X
;;   Get the ith element of this list
;;   (only defined for lists of i+1 or more elements)
;; - length : -> Natural
;;   Compute the number of elements in this list

;; empty is a [List X] for any X.
}

In other words, you have to make an object named @racket[empty] that
implements the list interface above.  Lists should work just like
we're used to, so for example, these tests should all pass if
@racket[empty] is appropriately defined:

@codeblock{
#lang class/1
(require "your-implementation-of-lists.rkt") ; provides empty

(define ls (empty . cons 'a . cons 'b . cons 'c . cons 'd . cons 'e))

(check-expect (empty . length) 0)
(check-expect (ls . length) 5)
(check-expect (ls . first) 'e)
(check-expect (ls . rest . first) 'd)
(check-expect (ls . rest . rest . first) 'c)
(check-expect (ls . rest . rest . rest . first) 'b)
(check-expect (ls . rest . rest . rest . rest . first) 'a)

(check-expect (ls . list-ref 0) 'e)
(check-expect (ls . list-ref 1) 'd)
(check-expect (ls . list-ref 2) 'c)
(check-expect (ls . list-ref 3) 'b)
(check-expect (ls . list-ref 4) 'a)
}

So now let's talk about Van Horn's idea.

Van Horn thinks if instead of representing a list as a ``list of
elements'' you could do better by representing a list as a ``forest of
trees of elements''. (A @deftech{forest} is just an arbitrarily long
sequence of trees.)  Moreover, the trees will get bigger and bigger as
you go deeper into the forest, and every tree is full. (A
@deftech{full tree} is a binary in which every node has a left and
right subtree that are full and of the same.) For the moment, don't
worry about @emph{why} this makes @racket[list-ref] fast---think about
that after you've implemented Van Horn's idea.

So here are the key invariants of a @deftech{quick list}:

@itemlist[
  @item{A quick list is a forest of increasingly large full binary trees.}
  @item{With the possible exception of the first two trees, every
    successive tree is @emph{strictly} larger.}
]

Now that we have the invariant, let's talk about the operations and
how they both can use and maintain the invariant.

First, @racket[first].  Since the list must be non-empty, we know the
forest has at least one tree, so we can get the first element of the
list by getting ``the first'' element of the tree, which for quick
lists, will be the top element.

Now, @racket[length].  If the forest is empty, the list has length
@racket[0].  If a forest has a tree, the length of the list is the
size of the tree plus the size of the rest of the forest.  (It's
useful to store the size of a tree separately from a tree so that you
don't have to compute it every time you need it.)

The @racket[list-ref] method works as follows: if the index is
@racket[0], the list must be non-empty, so take the first element,
i.e. the top element of the first tree in the forest.  If the index
is non-zero, there are two case: if it's less than the size of the
first tree, the element is in that tree, so fetch it from the first
tree.  If it's larger, adjust the index, and look in the remaining
trees of the forest.

To fetch an element from a tree: if the index is zero, the element is
the top element.  Otherwise, if the index is less than half the size,
it's on the left side; if the index is greater than half, it's on the
right.  (You might do yourself a favor a develop @racket[tree-ref] for
full binary trees and get it working and thoroughly tested before
attempting @racket[list-ref].)

These element-producing operations considered so far have used the
invariant.  Now let's turn to the list-producing operations which must
maintain it.

When an element is @racket[cons]ed, there are
two cases to consider:

@itemlist[

@item{If there at least two trees in the forest and the first two
trees are the same size, then make a new tree out of these two and
with the given element on top.  Here it is pictorially; we are given a
forest of full binary trees where the first two trees have the same
height:

@centered{
@image[#:scale .4 #:suffixes '(".pdf" ".png")]{figures/quick-lists1}}

To cons on the new element, we make a node that contains the element
and the first two trees as its left and right subtree:

@centered{
@image[#:scale .4 #:suffixes '(".pdf" ".png")]{figures/quick-lists2}}

Notice how this first tree is necessarily full, since the first two
trees were full and the same height; notice how this new first tree in
the forest is at most as large as the second tree (previously the
third tree).  These two observations demonstrate that the invariant
holds on the resulting forest, so @racket[cons] really makes a quick
list in this case.}

@item{Otherwise, we know that the size of the trees in the forest is
strictly increasing:

@centered{
@image[#:scale .4 #:suffixes '(".pdf" ".png")]{figures/quick-lists3}}

Therefore, we can just make a new tree with one element and make it
the first tree in the forest:

@centered{
@image[#:scale .4 #:suffixes '(".pdf" ".png")]{figures/quick-lists4}}

Notice how the one element tree is obviously full and that it is no
larger than the (now) second tree in the forest, so the invariant
holds in this case too.}]


To take the @racket[rest] of a list, there must be at least one tree
in the forest (since the list is non-empty):

@centered{
@image[#:scale .4 #:suffixes '(".pdf" ".png")]{figures/quick-lists2}}

We want to split this tree into its left and right and make these the
first two trees in the forest.  The element that was on top is dropped
on the floor and we're left with a representation of the rest of the
list:

@centered{
@image[#:scale .4 #:suffixes '(".pdf" ".png")]{figures/quick-lists1}}

And that's that.  When writing your code you want to make sure the
invariants are always true.  Good code should make this fact obvious;
bad code, not so much.

This is a nice little exercise in data structure design and
implementation, and although Van Horn @emph{wishes} this were really
his idea, he actually got it from reading a book by @index["Okasaki,
Chris"]{Chris Okasaki}, who has designed a bunch of these kinds of
data structures.  Go forth, and may your @racket[list-ref] never be
slow again.
