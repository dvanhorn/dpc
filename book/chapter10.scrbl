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

@title[#:tag "lec11"]{2/14: Mutation}

We want to design a class, @r[counter%], with the following interface

@codeblock{
;; m : -> Number
;; Produce the number of times `m' has been called
}

Now let's try to implement this class.  

@racketblock[
(define-class counter%
  (fields called)
  (define/public (m)
    #, @(elem "hmmm")))
]

Unfortunately, it's not immediately clear what to put in the body of
@r[m]. We can understand our task better by writing examples.  

@racketblock[
 (check-expect (send (counter% 0) m) 1)
 (check-expect (send (counter% 4) m) 5)
]

This suggests the following implementation implementation:

@filebox[@tt{counter%}]{
@racketblock[
 (define/public (m)
   (add1 (field called)))
]}

Now our all of our tests pass.

@(the-eval '(begin
	      (define-class counter%
		(fields called)
		(define/public (m) (add1 (field called))))
	      ))

However, when we try our a few more examples, we see this:

@interaction[#:eval the-eval
(define c (counter% 0))
(send c m)
(send c m)]

Of course, this is the wrong answer.  We shouldn't be surprised, since
nothing has changed about @r[c]---in fact, nothing ever happens to
@r[c], and only one @r[counter%] instance is produced in this
program.  
In order to give @r[m] the ability to remember things, we will need to
do something to get a different @r[counter%].

One possibility is to change @r[m] to produce both the desired result
@emph{and} a new counter.

@interaction[#:eval the-eval
(define-struct r (n new-counter))

(define-class counter%
  (fields called)
  (define/public (m)
    (make-r
     (add1 (field called))
     (counter% (add1 (field called))))))
(define c (counter% 0))

(send c m)

(define d (r-new-counter (send c m)))
d
(send d m)]

So far, so good---we can get a new @r[counter%], and when we use that
new value, we get the right answer.  However, we haven't solved the
problem yet:

@interaction[#:eval the-eval 
(send c m)]

This is the same answer that we had before, and not the desired one.

In fact, this behavior is the result of one of the important design
principles of this class, and of Fundies 1, up until this point.  If
you call a function or method with the same inputs, you get the same
result.  Always!

Unfortunately, that make it impossible to implement @r[m], because @r[m]s spec
violates this assumption---if you call it, it is required to produce a
@emph{different} result from the last time it was called.

Previously, we've always been able to rely on this test passing,
regardless of what you put in @r[E]

@racketblock[
(check-expect E E)
]

Actually, it turns out that there have been a few exceptions to this rule:
@itemlist[
@item{@racket[(random 5)]}
@item{User input, such as in @r[big-bang]}]

Now, however, we are proposing a much more fundament violation of this
principle. 

Before we violate the principle, though, let's look at one more
possible idea:  accumulators.

We could add an acummulator to @r[m], which is the previous number of
times we've been called. We've used this solution before to create
functions and methods that remember previous information.  In this
case, though, accumulators are a non-solution.  If we add an
accumulator to @r[m] to indicate what we're remembering, we get this
method:

@filebox[@r[counter%]]{
@racketblock[
(define/public (m accum) (add1 accum))]}

But that's a pretty boring method---it's just a wrapper around
@r[add1].  And it's not a solution to our problem: instead of the
@r[counter%] class or the @r[m] method remembering the number of
times we've called @r[m], we have to remember it ourselves, and
provide it as input every time we call @r[m].  


@section{Solution: changing the object}

To truly solve our problem, and implement @r[m], we need new language
support.  This support is provided in @racketmodname[class3].

The @racketmodname[class3] language provides the new @r[set-field!]
form, which is used like this:

@racketblock[
(set-field! f new-value)
]

This @emph{changes} the value of the field named @r[f] in @r[this]
object to whatever @r[new-value] is.  

We can now revise our defintion of @r[m] to 

@filebox[@tt{counter%}
@racketblock[
(define/public (m)
  (begin
   (set-field! called (add1 (field called)))
   (add1 (field called))))]]

Note that @r[set-field!] @emph{doesn't produce} a new version of the field,
instead it @emph{changes} the field named @r[called] to something new.

@margin-note{Question: How would we do something like this in a purely
		       functional language? 

		       Answer: We would do something similar to the
@r[make-r] approach presented above.  In
@link["http://haskell.org"]{Haskell}, this approach is frequently
used.}


We've also introduced one more language feature in
@racketmodname[class3]: @r[begin].  The @r[begin] form works by
evaluating each expression in turn, throwing away the result of every
expression except that last one.  Then it does the last part, and
produces that result.

@margin-note{Question: Do we have @r[begin0]?  

Answer: No.}

Unlike @r[set-field!], @r[begin] doesn't add any new capability to the
language.  For example, we can simulate @r[begin] using @r[local].
For example:

@racketblock[
(local [(define dummy (set-field! called (add1 (field called))))]
  (add1 (field called)))
]

This is very verbose, and requires creating new variables like
@r[dummy] that are never used.  Therefore, @r[begin] is a useful
addition to our language, now that we work with expressions like
@r[set-field!] that don't produce any useful results.  

@margin-note{@bold{A brief discussion of }@r[void]

What happens if we return the result of @r[set-field!]?  It produces
nothing---DrRacket doesn't print anything at all.

However, there's no way for DrRacket to truly have nothing at all, so
it has an internal value called @r[void].  This value doesn't have any
uses, though, and you shouldn't ever see it.}


Now Expressions do two things:
 - produce a result (everything does this)
 - has some effect (some expressions do this)

Now we write effect statements.  Have to write them for every
method/function that has an effect.
@verbatim|{

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

The differece is that @r[x] is the name of a number, and numbers don't
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

Question: Does @r[htdp] contain a copy of matthias, or are they all the
same matthias?  Answer: always the same, because we use the name @r[mf],
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

In the first expression, we *cannot* use @r[this], and we must produce
the result using @r[fields].  Later, we can use @r[this], and we get the
desired result.  }|