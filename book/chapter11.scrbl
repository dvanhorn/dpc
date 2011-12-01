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

@title[#:tag "lec12"]{2/17: Exam Review}

@section{Problem 1}

@verbatim|{
The hard part is translating english prose into a data defintion.
This is the most fundamental skill in CS.  

For ranges, here's what the spec is:
[lo,hi) -- a closed open range

Data Def: 
A Range is a (co% Number Number)

Then we add (lo,hi].

A Range is one of:
- (co% Number Number)
- (oc% Number Number)

Then we add:
  [4,5) union (6,8] union [100,101)

Thus we get the data def:

A Range is one of
- (co% Number Number)
- (oc% Number Number)
- (u% Range Range) ;; note recursive data def

Let's write the u% class:

(define-class u%
  (fields left right) ;; not min/max
  (define/public (in-range? n)
    (or (send (field left) in-range? n)
	(send (field right) in-range? n))))

How would we write @r[union]  in @r[co]?

(define/public (union r)
  (u% r this))

same in u% and oc%

@section{Problem 2: Shapes}

Data Def:

A Shape is one of:
- (rect% N N)
- (circ% N)

Part 1:
Write the bba (bounding-box-area) method.  Everyone did well on this.

Part 2: 
Abstract the two methods to a super class.

in rect%
(define/public (bba)
  (* (field width) (field height)))

in circ%
(define/public (bba)
  (sqr (* 2 (field radius))))

So, let's extend the Shape interface:

; Shape implements:
; width : -> N
;; compute the width of this shape
; height : -> N
;; compute the height 

Now we can rewrite our methods:

in rect%
(define/public (bba)
  (* (width) (height)))

in circ%
(define/public (bba)
  (* (width) (height)))

Now our methods are identical.  We can lift them up to a superclass:

(define-class shape%
  (define/public (bba)
    (* (width) (height))))

Except we need to change this to:

(define-class shape%
  (define/public (bba)
    (* (this . width) (this . height))))

Now we have to implement our interface in rect% and circ%

In rect%, we're already done -- define-class has generated the
accessor methods for us.

In circ%, we do this:

(define/public (width)
  (* 2 (field radius)))

(define/public (height)
  (* 2 (field radius)))
or
(define/public (height)
  (width))

Extend our data definintion:

A Shape is one of:
- (rect% N N)
- (circ% N)
- (square% N)

(define-class square%
  (super shape%)
  (fields width)
  (define/public (height) (field width)))

or, to be cuter (not necessarily a good idea on exams) :

(define-class square%
  (super rect%)
  (constructor (x) (fields x x)))

@section{Problem 3: Functions as Objects}

In this problem, we give you an interface:

An [IFun X Y] implements
apply : X -> Y
apply this function to the given input

Explanation of parametric interfaces.  Analogy to parametric data
definitions, ie [Listof X].  

What do we know about someting that implements [IFun Number String]?
It has an apply method which takes a Number and produces a String.

Let's consider Part 1 of this problem.

You're given an (X -> Y) function.  You should construct something
that implements [IFun X Y].  

So, we must construct a data definition, since we don't have any way
yet of constructing [IFun X Y].

; A [Fun X Y] is a (f% (X -> Y)) and implements [IFun X Y]

(define-class f%
  (fields f)
  ;; template
  (define/public (apply x)
    ... (field f) ... x))

code:

  (define/public (apply x)
    ((field f) x))

Now, on to part 2:

(define addone (f% add1))
> (addone . apply 3)
4
(define subone (f% sub1))
> (subone . apply 3)
2

Now, on to part 3:

We extend the interface with the compose method:

;; compose : [IFun Y Z] -> [IFun X Z]
;; produce a function that applies this function and then the given
;; function

Since we extended the interface, we must implement the compose method
in f%. 

(define/public (compose g)
  ... (field f) g (g . apply ...) (g . compose ...) ...)

remove the useless bits:

(define/public (compose g)
  ... (field f) (g . apply ...) ...)

First, we need to construct an [IFun X Z].  But the only way to do
that is with (f% ...), which requires a function as input:

(define/public (compose g)
  (f% (lambda (x)
	... (field f) 
	... (g . apply ...)  ...)))

Now our inventory is extended:

(define/public (compose g)
  (f% (lambda (x)
	... x ;; contract: X
	... (field f) ;; contract : X -> Y
	... (g . apply ...)  ;; contract : Y -> Z
	...)))

(define/public (compose g)
  (f% (lambda (x)
	(g . apply ((field f) x)))))


Possible mistakes:

(define/public (compose g)
  (f% (lambda (x)
	(g ((field f) x)))))

g is not a function, this doesn't work

(define/public (compose g)
  (f% (lambda (x)
	(g . f ((field f) x)))))

f is not in the [IFun Y Z] interface, this doesn't work

@section{Problem 4: Queues}

This problem is all about maintaining invariants.  

@codeblock{
;; A [IQ X] implements:
;;
;; head : -> X
;; Produce the element at the head of this queue.
;; Assume: this queue is not empty.
;;
;; deq : -> [IQ X]      (Short for "dequeue")
;; Produces a new queue like this queue, but without 
;; this queue's first element.
;; Assume: this queue is not empty.
;;
;; enq : X -> [IQ X]    (Short for "enqueue")
;; Produce new queue with given element added to the
;; END of this queue.
;;
;; emp? : -> Boolean
;; Is this queue empty?
}

here's the representation idea:

front: (list 'x 'y)
rear: (list 'z 'p 'q) -- reverse order

So this has the elements in this order:
  'x 'y 'q 'p 'z

Invariants have dual roles: they give us things to ensure, and things
we can rely on.

Let's consider one case where we can simply rely on the invariant.

;; A [Q% X] is a (q% [Listof X] [Listof X])
(define/class q%
  (fields front rear)
  (define/public (emp?)
    (empty? (field front)))
  ...
)

The @r[head] method is another such case.
We know, from the assumption, that the queue isn't empty.  From the
invariant, we therefore know that the front isn't empty.  
So we just get:
(define/public (head)
  (first (field front)))

Now let's consider a case where we have to ensure the invariant -- the
@r[enq] method.

(define/public (enq x)
  (q% (field front) (cons x (field rear))))

But what if the queue is empty?  We just broke the invariant.  Whoops.

(define/public (enq x)
  (cond [(emp?)
	 (q% (list x) empty)]
	[else
	 (q% (field front) (cons x (field rear)))]))

Now we maintain the invariant in both cases.

Now we get to the trickiest method, @r[deq].  Here's a simple way of
doing it.

(define/public (deq)
  (q% (rest (field front))
      (field rear)))

But what if there's only one person?  Then we're ok, because the whole
queue is empty?  

What if there's only one person in the front, but a million people in
the rear?  Then we've broken the invariant.  

We need to distinguish the two possible cases:

(define/public (deq)
  (cond [(empty? (rest (field front)))
	 (q% (reverse (field rear))
	     empty)]
	[else (q% (rest (field front))
		  (field rear))]))
part 2:

(q% '(x y z) '(p q r)) has the same interp as
(q% '(x y) '(p q r z)) but different representations

Now we define a to-list method that does the interpretation for
testing purposes.

;; to-list : -> [Listof X]
;; produce a list of the elements of this queue in order
(define/public (to-list)
  (append (field front) (reverse (field rear))))

Now we can use this to test that the interp of the two queue in our
example is the same.  

part 3:
Write a constructor.

In the q% class:
(constructor (f r)
   (cond [(empty? f)
	  (fields (reverse r) empty)]
	 [else (fields f r)]))

Various other possibilites.  

(constructor (f r)
  (fields (append f (reverse r)) empty))

@section{Problem 5: Properties and Dictionaries}


@codeblock{
;; A Dict is one of:
;;  - (ld-empty%)
;;  - (ld-cons% Nat String Dict)
;; implements:
;;
;; has-key? : Nat -> Boolean
;; Does this dict. have the given key?
;;
;; lookup : Nat -> String
;; Produce the value associated with the given key in this dict.
;; Assume: the key exists in this dict.
;;
;; set : Nat String -> Dict
;; Associate the given key to the given value in this dict.

(define-class ld-empty%
  (define/public (has-key? k) false)

  (define/public (set k v)
    (ld-cons% k v this)))

(define-class ld-cons%
  (fields key val rest)

  (define/public (has-key? k)
    (cond [(= (field key) k) true]
          [else ((field rest) . has-key? k)]))

  (define/public (lookup k)
    (cond [(= (field key) k) (field val)]
          [else ((field rest) . lookup k)]))

  (define/public (set k v)
    (ld-cons% k v this)))
}

In this problem, we're not asking you to develop new code, but to
reason about existing code.   You need to develop properties
that the dictionary implmentation has, and then embody those
properties as predicates that we can test.  

First, the "set/lookup" property:

(d . set n s . lookup n) should produce s.  As a predicate:

;; Dict Number String -> Boolean
;; check the set/lookup property on these inputs
(define (set/lookup? d n s) (string=? (d . set n s . lookup n) s))

Now, we can use this to write tests.

(check-expect (set/lookup? (ld-empty%) 3 "Wilma") true)
(check-expect (set/lookup? (ld-empty%) 17 "Fred") true)

Then, we consider a generalization of this property to consider
multiple keys and values.  

(define (2-set/lookup? d n s m t)
  (and 
   (string=? (d . set n s . set m t . lookup n)
	     s)
   (string=? (d . set n s . set m t . lookup m)
	     t)))

Here's an example where this works:

(check-expect (2-set/lookup? (ld-empty%) 3 "Fred" 4 "Wilma") true)
 
}|