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

@title[#:tag "lec10"]{2/14: Mutation}

I want to design a class, counter%, which has a method called m.

m : -> Number
the number of times m has been called

@racketblock[
(define-class counter%
  (fields called)
  (define/public (m)
    #, @(elem "hmmm")))


@racketblock[
 (check-expect (send (counter% 0) m) 1)
]

This suggests the implementation:

@racketblock[
 (define/public (m)
   (add1 (field called)))
]

Now our tests passed.

But:

(define c (counter% 0))
(c . m)
(c . m)

Wrong answer!

We want to give `m' the ability to remember things.

We could change `m' to produce both the result and a new counter.

(define-struct r (n new-counter))

(define/public (m)
  (make-r
   (add1 (field called))
   (counter% (add1 (field called)))))

(c . m)

(define d (r-new-counter (c . m)))
d
(d . m)

But:

(c . m)  still produces the same answer.

THis is one of the important design principles of this class, and
Fundies 1, up until this point.  If you call a function or method with
the same inputs, you get the same result.  Always!

But that make it very hard to implement `m', because `m's spec
violates this assumption.

Previously, we've always had this be true:

(check-expect E E)

Exceptions:
* (random 5)
* User input

But now we are proposing a much more fundament violation.  

One (non)-solution: accumulators.

We could add an acummulator to m, which is the previous number of
times we've been called. But that's a pretty boring function---it's
just add1.  

** Solution: changing the object

Swith to class3.  

Now we have the `set-field!' form.

We can change our defintion to 

(define/public (m)
  (begin
   (set-field! called (add1 (field called)))
   (add1 (field called))))

`set-field!' doesn't produce something new, instead it changes the
field named called to something new.  

How does this work in a purely function language?  See the `make-r'
version.

How does `begin' work?  It evaluates each expression in turn, throws
all the non-last expressions away.  Then it does the last part, and
produces that result.

Do we have begin0?  No.

Can we do begin-like things without begin?  Yes, use local.  

What happens if we return the result of `set-field!'?  It produces
nothing.  Some discussion of `void' here.

Now Expressions do two things:
 - produce a result (everything does this)
 - has some effect (some expressions do this)

Now we write effect statements.  Have to write them for every
method/function that has an effect.

@codeblock{
;; m : -> Number
;; Produce the number of times m has been called
;; Effect : increment the called field
(define/public (m)
  (begin
   (set-field! called (add1 (field called)))
   (add1 (field called))))
}

We've lost a lot of reasoning power but gained expressiveness.

What have I really gained, though?

Bank accounts.

Imagine that you're modeling bank financial systems.  You want to
deposit money into the account, and then the money should be there
afterwards.  

;; An Account is (account% Number)
(define-class account%
  (fields amt)
  
  ;; Number -> Account
  (define/public (deposit n)
    (account% (+ (field amt) n))))

But this doesn't model bank accounts properly.  

I deposit, my valentine deposits, I deposit -- whoops!

New version:

;; An Account is (account% Number)
(define-class account%
  (fields amt)
  
  ;; Number -> 
  ;; Effect: increases the field amt by n
  ;; Purpose: add money to this account
  (define/public (deposit n)
    (set-field! amt (+ (field amt) n))))

Note that we don't need to produce any result at all.  

;; A Person is (person% String Account Number)
(define-class person%
  (fields name bank paycheck)
  ;; -> Void
  ;; Deposit the appropriate amount
  ;; Effect: changes the the bank account amt
  (define/public (pay)
    (send (field bank) deposit (field paycheck))))

(define dvh-acct (account% 0))
(define dvh (person% "DVH" dvh-acct 150))
(define sweetie (person% "Sweetie" dvh-acct 3000))

(send dvh pay)
dvh-acct
(send sweetie pay)
dvh-acct

Note that we CANNOT replace dvh-acct with (account% 0) -- we'd get
totally different results.  

Now equality is much more subtle -- intensional equality vs
extensional equality. Same fork example.  

What if we do:

(define new-acct dvh-acct)
(define p (person% "Fred" new-acct 400))
(p . pay)
dvh-acct ;; updated

What if we create new account% with 0?  Then the effects are not
shared.  

What if we do:
(define x (dvh-acct . amt))
x
(dvh . pay)
x ;; still the same

What if we do

(define y (dvh . bank))
y
(dvh . pay)
y ;; now different

The differece is that `x' is the name of a number, and numbers don't
change, but y is the name of an account, and accounts change over
time.  

Objects can change, but other things do not change.  Structures and
lists can point to objects that change, but the structures and lists
themselves do not change, the object they point to are the same
objects.  

Testing is hard with mutation.  Give an example in the notes.  


Circular Data:

Books & Authors

Books have:
title : String
author : Author

Authors have:
name : String
books : [Listof Book]

As data def:

A Book is (book% String Author)
An Author is (author% String [Listof Book])

Can we make an author?

(author% "DVH" empty)
(book% "I Love Flow Analysis" (author% "DVH" empty)

But this is wrong: DVH has written a book, but the author object
doesn't know about it.  

Picture of HTDP/Matthias that is hard to capture in the notes.  

Do we need books to know the author? Yes.

We've seen this before with graph structure.  We represented graphs as
association lists, using symbolic names. 

;; A Book is (book% String Author)
(define-class book%
  (fields title author))

;; An Author is (author% String [Listof Book])
(define-class author%
  (fields name books))

(define mf (author% "MF" empty))
(define htdp (book% "The Blue Book" mf))

But:

> htdp
> mf

Question: Does `htdp' contain a copy of matthias, or are they all the
same matthias?  Answer: always the same, because we use the name `mf',
we didn't construct a new one.  

Let's add a new method for modifying the author after a book is
written:

(define/public (add-book b)
  (set-field! books (cons b (field books))))

Now we change our example:

(define mf (author% "MF" empty))
(define htdp (book% "The Blue Book" mf))
(mf . add-book htdp)

How does it print? 

See graph-style printing.

But every times we construct a book with an author, we want to use
`add-book'.  So, let's use the constructor.  

(constructor (t a)
  (fields t a)
  (a . add-book this))

In the first expression, we *cannot* use `this', and we must produce
the result using `fields'.  Later, we can use `this', and we get the
desired result.  