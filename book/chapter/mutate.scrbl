#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/1 check-expect define-struct ... length
                                numerator denominator))
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

@title{Ch-Ch-Ch-Ch-Changes}

We want to design a class, @r[counter%], with the following interface

@classblock{
;; m : -> Number
;; Produce the number of times `m' has been called
}

Now let's try to implement this class.  

@racketblock[
(define-class counter%
  (fields called)
  (define (m)
    #, @(elem "hmmm")))
]

Unfortunately, it's not immediately clear what to put in the body of
@r[m]. We can understand our task better by writing examples.  

@classblock{
(check-expect ((counter% 0) . m) 1)
(check-expect ((counter% 4) . m) 5)
}

This suggests the following implementation:

@filebox[@r[counter%]]{
@classblock{
(define (m)
  (add1 (this . called)))
}}

Now our all of our tests pass.

@(the-eval '(begin
              (define-class counter%
                (fields called)
                (define (m) (add1 (send this called))))
              ))

However, when we try our a few more examples, we see this:

@interaction[#:eval the-eval
(define c (counter% 0))
(send c m)
(send c m)
]

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
  (define (m)
    (make-r
     (add1 (send this called))
     (counter% (add1 (send this called))))))
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
(define (m accum) (add1 accum))]}

But that's a pretty boring method---it's just a wrapper around
@r[add1].  And it's not a solution to our problem: instead of the
@r[counter%] class or the @r[m] method remembering the number of
times we've called @r[m], we have to remember it ourselves, and
provide it as input every time we call @r[m].  

@;SOLN

To truly solve our problem, and implement @r[m], we need new language
support.  This support is provided in @racketmodname[class/3].

The @racketmodname[class/3] language provides the new @r[set-field!]
form, which is used like this:

@racketblock[
(set-field! f new-value)
]

This @emph{changes} the value of the field named @r[f] in @r[this]
object to whatever @r[new-value] is.  

We can now revise our defintion of @r[m] to 

@filebox[@r[counter%]]{
@racketblock[
(define (m)
  (begin (set-field! called (add1 (send this called)))
         (add1 (send this called))))]}

Note that @r[set-field!] @emph{doesn't produce} a new version of the field,
instead it @emph{changes} the field named @r[called] to something new.

@margin-note{Question: How would we do something like this in a purely
                       functional language? 

                       Answer: We would do something similar to the
@r[make-r] approach presented above.  In
@link["http://haskell.org"]{Haskell}, this approach is frequently
used.}


We've also introduced one more language feature in
@racketmodname[class/3]: @r[begin].  The @r[begin] form works by
evaluating each expression in turn, throwing away the result of every
expression except that last one.  Then it does the last part, and
produces that result.

@margin-note{Question: Do we have @r[begin0]?  

Answer: No.}

Unlike @r[set-field!], @r[begin] doesn't add any new capability to the
language.  For example, we can simulate @r[begin] using @r[local].
For example:

@racketblock[
(local [(define dummy (set-field! called (add1 (send this called))))]
  (add1 (send this called)))
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

@filebox[@r[counter%]]{
@classblock{
;; m : -> Number
;; Produce the number of times m has been called
;; Effect : increment the called field
(define (m)
  (begin (set-field! called (add1 (send this called)))
         (add1 (send this called))))
}}

We've lost a lot of reasoning power but gained expressiveness.

What have I really gained, though?

Imagine that you're modeling bank financial systems.  You want to
deposit money into the account, and then the money should be there
afterwards.  

@classblock{
;; An Account is (account% Number)
(define-class account%
  (fields amt)
  
  ;; Number -> Account
  (define (deposit n)
    (account% (+ (this . amt) n))))
}

But this doesn't model bank accounts properly.  

I deposit, my valentine deposits, I deposit -- whoops!

New version:

@classblock{
;; An Account is (account% Number)
(define-class account%
  (fields amt)
  
  ;; Number -> Void
  ;; Effect: increases the field amt by n
  ;; Purpose: add money to this account
  (define (deposit n)
    (set-field! amt (+ (this . amt) n))))
}
Note that we don't need to produce any result at all.  


@classblock{
;; A Person is (person% String Account Number)
(define-class person%
  (fields name bank paycheck)
  ;; -> Void
  ;; Deposit the appropriate amount
  ;; Effect: changes the the bank account amt
  (define (pay)
    (this . bank . deposit (this . paycheck))))
}

@(the-eval
  '(module a class/3
     (provide account% person%)
     ;; An Account is (account% Number)
     (define-class account%
       (fields amt)
  
       ;; Number -> 
       ;; Effect: increases the field amt by n
       ;; Purpose: add money to this account
       (define (deposit n)
         (set-field! amt (+ (send this amt) n))))

     ;; A Person is (person% String Account Number)
     (define-class person%
       (fields name bank paycheck)
       ;; -> Void
       ;; Deposit the appropriate amount
       ;; Effect: changes the the bank account amt
       (define (pay)
         (send (send this bank) deposit (send this paycheck))))))

@(the-eval '(require 'a))

@interaction[#:eval the-eval
(define dvh-acct (account% 0))
(define dvh (person% "DVH" dvh-acct 150))
(define sweetie (person% "Sweetie" dvh-acct 3000))

(send dvh pay)
dvh-acct
(send sweetie pay)
dvh-acct
]


Note that we @emph{cannot} replace @r[dvh-acct] with @r[(account% 0)]
-- we'd get totally different results.

Now equality is much more subtle -- intensional equality vs
extensional equality. Same fork example.  

What if we do:
@#reader scribble/comment-reader
(interaction #:eval the-eval
(define new-acct dvh-acct)
(define p (person% "Fred" new-acct 400))
(send p pay)
;; updated
dvh-acct 
)

What if we create new account% with 0?  Then the effects are not
shared.  

What if we do:
@#reader scribble/comment-reader
(interaction #:eval the-eval
(define x (send dvh-acct amt))
x
(send dvh pay)
;; still the same
x 
)

What if we do
@#reader scribble/comment-reader
(interaction #:eval the-eval
(define y (send dvh bank))
y
(send dvh pay)
;; now different
y
)

The differece is that @r[x] is the name of a number, and numbers don't
change, but y is the name of an account, and accounts change over
time.  

Objects can change, but other things do not change.  Structures and
lists can contain objects that change, but the structures and lists
themselves do not change, the object they point to are the same
objects.  

Testing is hard with mutation.  Give an example in the notes.  


