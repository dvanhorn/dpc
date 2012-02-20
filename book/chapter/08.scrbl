#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct check-expect ...))
          (for-label (except-in class/1 define-struct ... length check-expect filter))
	  (for-label 2htdp/image)
	  (for-label class/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require racket/port))
    ;; Ignore all testing output
    (the-eval '(current-output-port (open-output-nowhere)))
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class/1))
    the-eval))

@(the-eval '(require book/chapter/08/marathon))


@title[#:tag "chapter:functions"]{Abstraction via Functions}

Functions can play the role of an abstraction mechanism.  They can
also play the role of a class of data definitions.  In this chapter we
explore both roles in the context of object-oriented programs by
representing functions with objects.

@section{Functions as objects: abstracting predicates}

The Boston marathon is the world oldest annual marathon and is
Boston's most widely viewed sporting event.  Suppose the Boston
Athletic Association hired you to write software that wrangled data
about their nearly 40,000 finishers.  A reasonable representation
would be a list of runners, where the BAA wants to track the following
about each runner:

@itemlist[
@item{Name}
@item{Age (in years)}
@item{Bib number}
@item{Gender}
@item{Finish time (in seconds)}
]

That leads to following data definitions:

@classblock{
;; A LoR is one of:
;; - (new mt%)
;; - (new cons% Runner)
(define-class mt%)
(define-class cons%
  (fields first rest))

;; A Runner is a 
;;   (new runner% String Natural Natural Natural Gender).
(define-class runner%
  (fields name age bib time gender))

;; A Gender implements:
;; - is-male? : -> Boolean
;; - is-female? : -> Boolean
}

Here's a simple implementation of @tt{Gender}:

@classblock{
(define-class male%   ; implements Gender
  (define (is-male?) true)
  (define (is-female?) false))
(define-class female% ; implements Gender
  (define (is-male?) false)
  (define (is-female?) true))

(define m (new male%))
(define f (new female%))
}

Here are a few examples of runners:

@classblock{
(define johnny (new runner% "Kelley" 97 1001 (* 351 60) m))
(define bobby  (new runner% "Cheruiyot" 33 8 (* 127 60) m))
(define roberta (new runner% "Gibb" 23 121 (* 200 60) f))

(define mt (new mt%))
(define rs
  (new cons% johnny (new cons% bobby (new cons% roberta mt))))
}



Now let's consider a couple computations the BAA might ask you to
support in their program.  One taks is to figure calculate a list of
``fast runners,'' which we'll take as runners finishing in under three
hours.  That's a straightforward computation:

@filebox[@racket[mt%]]{
@classblock{
;; fast : -> LoR
;; Runners in this empty list with times under three hours.
(define (fast) this)
}}

@filebox[@racket[mt%]]{
@classblock{
;; fast : -> LoR
;; Runners in this non-empty list with times under three hours.
(define (fast) 
  (cond [(< (this . first . time) (* 180 60))
         (new cons% 
              (this . first)
              (this . rest . fast))]
	[else
	 (this . rest . fast)]))                   
}}


@examples[#:eval the-eval
(eval:alts (mt . fast) (send mt fast))
(eval:alts (rs . fast) (send rs fast))]

A similar method the BAA might want is one to compute the list of all
runners over 50 years of age:

@filebox[@racket[mt%]]{
@classblock{
;; old : -> LoR
;; Runners in this empty list with age over 50.
(define (old) this)
}}

@filebox[@racket[cons%]]{
@classblock{
;; old : -> LoR
;; Runners in this non-empty list with age over 50.
(define (old) 
  (cond [(> (this . first . age) 50)
         (new cons% 
              (this . first)
              (this . rest . old))]
	[else
	 (this . rest . old)]))                   
}}

@examples[#:eval the-eval
(eval:alts (mt . old) (send mt old))
(eval:alts (rs . old) (send rs old))]

These methods look so similar it's natural to wonder if we can't
extract their differences and express them as the instantiation of
some more general method.  On closer inspection, the two method differ
in predicate use to select whether a given runner is in the result
list or not:

@itemlist[
@item{For @racket[fast], the predicate is ``is the runner's time less
than 3 hours?''}

@item{For @racket[old], the predicate is ``is the runner's age more
than 50?''}
]

One natural design is to take write a method that encapsulates the
similarities of @racket[fast] or @racket[old], say @racket[filter],
and add a parameter that consumes the differences, that is, the
predicate, represented as a function @tt{[Runner -> Boolean]}.  The
original methods can be recreated by supplying the appropriate
function to the @racket[filter] method, respectively:

@itemlist[
@item{@racket[(λ (r) (< (this #,dot time) (* 180 60)))]}
@item{@racket[(λ (r) (> (this #,dot age) 50))]}
]

But what if we wanted to reresent these predicates with @emph{objects}?
Is such a thing possible?

Since both of these predicates are really asking questions of runners,
it may be tempting to thinking of the predicate as something that
lives in the @tt{Runner} class, and certainly we could define
@racket[fast?] and @racket[old?] methods in @racket[runner%].  But
that wouldn't help to define a single @racket[filter] method since we
cannot parameterize a method by a method @racket[name]---the
arguments of a method must @emph{values}.

We could instead pass in an @emph{object that contains a method}.  In
other words, we can represent the predicate as an object with a single
@racket[ask] method that consumes a runner and produces a
Boolean (just like the functions above).  

To represent the ``is fast?'' predicate, we define a class:

@classblock{
(define-class is-fast%
  ;; ask : Runner -> Boolean
  ;; Is the given runner fast (time less than 250)?  
  (check-expect ((new is-fast%) . ask johnny) false)
  (check-expect ((new is-fast%) . ask bobby) true)
  (define (ask r)
    (< (r . time) (* 180 60))))
}

Likewise, to define the ``is old?'' predicate, we define another class:

@classblock{
(define-class is-old%
  ;; ask : Runner -> Boolean
  ;; Is the given runner older than 50?
  (check-expect ((new is-old%) . ask johnny) true)
  (check-expect ((new is-old%) . ask roberta) false)  
  (define (ask r)
    (> (r . age) 50)))
}

The @racket[(new is-fast%)] and @racket[(new is-old%)] values can be
the arguments of the abstraction of @racket[fast] and @racket[old]:

@filebox[@racket[mt%]]{
@classblock{
;; filter : {ask : Runner -> Boolean} -> LoR
;; Runners in this empty list satisfying given predicate.
(define (filter q) this)
}}

@filebox[@racket[cons%]]{
@classblock{
;; filter : {ask : Runner -> Boolean} -> LoR
;; Runners in this non-empty list satisfying given predicate.
(define (filter q) 
  (cond [(q . ask (this . first))
         (new cons% 
              (this . first)
              (this . rest . filter q))]
	[else
	 (this . rest . filter q)]))
}}

What's more, you'll notice when you recreate @racket[fast] and
@racket[old] each have duplicated, identical definitions in
@racket[mt%] and @racket[cons%], which means they can be lifted to a
super class:

@filebox[@racket[list%]]{
@classblock{
;; fast : -> LoR
;; Runners in this list with times less than 3 hours.
(define (fast) (this . filter (new is-fast%)))

;; old : -> LoR
;; Runners in this list with age more than 50 years.
(define (old) (this . filter (new is-old%)))
}}

At this point, it's easy to create new predicates on runners and use
them to filter a list of runners.  For example, if we want to select
the list of male or female runners:

@classblock{
(define-class is-male%
  ;; ask : Runner -> Boolean
  ;; Is the given runner male?
  (check-expect ((new is-male%) . ask johnny) true)
  (check-expect ((new is-male%) . ask roberta) false)
  (define (ask r)
    (r . gender . is-male?)))

(define-class is-female%
  ;; ask : Runner -> Boolean
  ;; Is the given runner female?
  (check-expect ((new is-female%) . ask johnny) false)
  (check-expect ((new is-female%) . ask roberta) true)
  (define (ask r)
    (r . gender . is-female?)))
}

@examples[#:eval the-eval
(eval:alts (rs #,dot filter (new is-female%)) (send rs filter (new is-female%)))
]


Of course, these are pretty simple predicates.  Let's attack something
a little more ambitious.  The Boston Marathon doesn't let just anybody
enter the race.  You have to qualify.  One way to quality for the
Boston Marahon is to run under a certain time in a previous Boston
Marathon (the magic of recursion at work!).  Let's write a predicate
that will tell us the list of runners that will qualify for next
year's marathon.  The BAA qualification standards for 2013 are:

@verbatim|{
Age     Men             Women
-----   ----------      -----------
18-34 	3hrs 5min 	3 hrs 35min
35-39 	3hrs 10min 	3 hrs 40min
40-44 	3hrs 15min 	3 hrs 45min
45-49 	3hrs 25min 	3 hrs 55min
50-54 	3hrs 30min 	4 hrs 0min
55-59 	3hrs 40min 	4 hrs 10min
60-64 	3hrs 55min 	4 hrs 25min
65-69 	4hrs 10min 	4 hrs 40min
70-74 	4hrs 25min 	4 hrs 55min
75-79 	4hrs 40min 	5 hrs 10min
80+ 	4hrs 55min 	5 hrs 25min
}|

It's tempting to write a monolithic @racket[qualify%] predicate that
includes all of logic embodied in the above table, but let's instead
break it down into smaller pieces.

If we just focus on a single row, we can see that each row could form
its own predicate.  For example, the first runner is ``is the runner
over 18 and younger than 35 and, if male, has a time less than 3hrs
5min, ...?''  Even this predicate can be broken down into smaller
peices: ``is the runner older than 18 and younger than 35?'', ``is the
runner male?'', ``is the runner's time less than 3hrs 5min?'', etc.
And among these simple questions, we can observe there are really
related @emph{families} of questions we might ask: ``is the runner
older than @emph{N} and younger than @emph{M}?'', for example.

Let's start with the age.  We can represent the age group predicates
with a functional object that contains data:

@classblock{
(define-class in-age%
  (fields lo hi)
  ;; Is the given runner within [lo,hi] age?
  (check-expect ((new in-age% 18 34) . ask roberta) true)
  (check-expect ((new in-age% 18 34) . ask johnny) false)
  (define (ask r)
    (<= (this . lo) (r . age) (this . hi))))
}

Likewise we can represent the ``faster than @emph{N} time?'' predicate
as a functional object with data:

@classblock{
(define-class faster%
  (fields hrs mins)
  ;; ask : Runner -> Boolean
  ;; Is the given runner's time faster than hrs:mins?
  (check-expect ((new faster% 3 35) . ask roberta) true)
  (check-expect ((new faster% 3 35) . ask johnny) false)
  (define (ask r)
    (< (r . time) (* 60 (+ (* 60 (this . hrs)) (this . mins))))))
}

The ``is the runner male?'' and ``female?'' predicates are
straightforward:

@classblock{
(define-class is-male%
  ;; ask : Runner -> Boolean
  ;; Is the given runner male?
  (check-expect ((new is-male%) . ask johnny) true)
  (check-expect ((new is-male%) . ask roberta) false)
  (define (ask r)
    (r . gender . is-male?)))

(define-class is-female%
  ;; ask : Runner -> Boolean
  ;; Is the given runner female?
  (check-expect ((new is-female%) . ask johnny) false)
  (check-expect ((new is-female%) . ask roberta) true)
  (define (ask r)
    (r . gender . is-female?)))
}

Now we have the small peices to say things like ``the given runner
qualifies if they are 18-34 years-old, a woman, and have a running
time less than 3 hours and 35 minutes'', but we lack the ability to
put them together to form the overall predicate.  In particular, we
are lacking the ability to combine predicates with ``and'', ``or'',
``implies'', etc.  But what's an ``and''?  It's really just a
predicate built out of predicates:

@classblock{
(define-class and%
  (fields q1 q2)
  ;; ask : Runner -> Boolean
  ;; Does the given runner satisfy q1 *and* q2?
  (check-expect ((new and% f? young?) . ask roberta) true)
  (check-expect ((new and% f? young?) . ask bobby) false)
  (check-expect ((new and% f? young?) . ask johnny) false)
  (define (ask r)
    (and (this . q1 . ask r)
         (this . q2 . ask r))))
}

The test cases make use of the following definitions for succinctness:

@classblock{
(define f? (new is-female%))
(define young? (new in-age% 18 34))
}

Now we can construct the predicate ``is the given runner 18-34, a
woman, and did she finish in under 3 hours and 35 minutes?'':

@classblock{
(new and% (new in-age% 18 34) 
          (new and% (new is-female%) (new faster% 3 35)))
}

And from here, it's easy to express the predicate of a row (assuming
you can define @racket[or%]):

@classblock{
(define-class qrow%
  (fields lo hi m-hrs m-mins f-hrs f-mins)
  ;; ask : Runner -> Boolean
  ;; Does given runner satifisy conditions of this row?
  (check-expect ((new qrow% 18 34 3 05 3 35) . ask roberta) true)
  (check-expect ((new qrow% 18 34 3 05 3 35) . ask bobby) true)
  (check-expect ((new qrow% 18 34 3 05 3 35) . ask johnny) false)
  (define (ask r)
    ((new and% 
	  (new in-age% (this . lo) (this . hi))
	  (new or% 
	       (new and% 
		    (new is-male%)
		    (new faster% (this . m-hrs) (this . m-mins)))
	       (new and%
		    (new is-female%)
		    (new faster% (this . f-hrs) (this . f-mins)))))
     . ask r)))
}

Only the last row, which asks about being 80+ is a different (here, we
assume you can define @racket[older%] with the obvious meaning):

@classblock{
(define-class lastrow%
  (fields lo m-hrs m-mins f-hrs f-mins)
  ;; ask : Runner -> Boolean
  ;; Does given runner satifisy conditions of this last row?
  (check-expect ((new lastrow% 80 4 55 5 10) . ask roberta) false)
  (check-expect ((new lastrow% 80 4 55 5 10) . ask bobby) false)
  (check-expect ((new lastrow% 80 4 55 5 10) . ask johnny) false)
  (define (ask r)
    ((new and% 
	  (new older% (this . lo))
	  (new or% 
	       (new and% 
		    (new is-male%)
		    (new faster% (this . m-hrs) (this . m-mins)))
	       (new and%
		    (new is-female%)
		    (new faster% (this . f-hrs) (this . f-mins)))))
     . ask r)))
}

And now the @racket[qualify%] class is easy and exactly matches the
structure of the BAA's table:

@classblock{
(define-class qualify%
  ;; ask : Runner -> Boolean
  ;; Does given runner qualify for next Boston Marathon?
  (check-expect ((new qualify%) . ask roberta) true)
  (check-expect ((new qualify%) . ask bobby) true)
  (check-expect ((new quality%) . ask johnny) true)
  (define (ask r)
    (or ((new qrow% 18 34 3 05 3 35) . ask r)
	((new qrow% 35 39 3 10 3 40) . ask r)
	((new qrow% 40 44 3 15 3 45) . ask r)
	((new qrow% 45 49 3 25 3 55) . ask r)
	((new qrow% 50 54 3 30 4 00) . ask r)
	((new qrow% 55 59 3 40 4 10) . ask r)
	((new qrow% 60 64 3 55 4 25) . ask r)
	((new qrow% 65 69 4 10 4 40) . ask r)
	((new qrow% 70 74 4 25 4 55) . ask r)
	((new qrow% 75 79 4 40 5 10) . ask r)
	((new lastrow% 80 4 55 5 10) . ask r))))
}

Now it's easy to filter the list of runners for those qualifying for
next year's race.  Unfortunately, Johnny doesn't make the cut, but
considering he's 97 and won the 1935 and 1945 Boston Marathon, perhaps
the BAA will cut him some slack.

@examples[#:eval the-eval
(eval:alts (rs #,dot filter (new qualify%) #,dot filter (new is-male%)) 
	   (send (send rs filter (new qualify%)) filter (new is-male%)))
(eval:alts (rs #,dot filter (new qualify%) #,dot filter (new is-female%)) 
	   (send (send rs filter (new qualify%)) filter (new is-female%)))
]

@section{Functions as objects: abstracting comparisons}

@section{Functions as data as objects: infinite sequences}

Functions were a useful abstraction mechanism.

@#reader scribble/comment-reader
(racketblock
  ;; An ISequence implements 
  ;; - term : Natural -> Number
  ;;   compute the i^th element of this sequence

  (define-class even%
    ;; term : Natural -> Natural
    ;; compute the i^th even number
    (check-expect ((even%) #,(racketidfont ".") term 7) 14)
    (define/public (term i)
      (* i 2)))

  (define-class odd%
    ;; term : Natural -> Natural
    ;; compute the i^th odd number
    (check-expect ((odd%) #,(racketidfont ".") term 7) 15)
    (define/public (term i)
      (add1 (* i 2))))

  (define-class even-series%
    ;; term : Natural -> Natural
    ;; sum up the first n even numbers
    (check-expect ((even-series%) #,(racketidfont ".") term 10) 90)
    (define/public (term n)
      (cond [(zero? n) 0]
            [else (+ ((even%) #,(racketidfont ".") term (sub1 n))
		     (term (sub1 n)))])))

  (define-class odd-series%
    ;; term : Natural -> Natural
    ;; sum up the first n odd numbers
    (check-expect ((odd-series%) #,(racketidfont ".") term 10) 100)
    (define/public (term n)
      (cond [(zero? n) 0]
            [else (+ ((odd%) #,(racketidfont ".") term (sub1 n))
		     (term (sub1 n)))])))
)

We can now apply the process for designing abstractions with 
functions-as-values from
@link["http://htdp.org/2003-09-26/Book/curriculum-Z-H-28.html#node_sec_22.2"]{
@emph{HtDP} Section 22.2}
adapted for functions-as-objects.

@#reader scribble/comment-reader
(racketblock  
  (define-class series%
    ;; Σ : ISequence -> ISequence
    ;; Make sequence summing up first n numbers of sequence
    (check-expect ((series%) #,(racketidfont ".") Σ (even%) #,(racketidfont ".") term 10) 90)
    (check-expect ((series%) #,(racketidfont ".") Σ (odd%) #,(racketidfont ".") term 10) 100)
    (define/public (Σ seq)
      (local [(define-class s%
                (define/public (term n)
		  (cond [(zero? n) 0]
			[else (+ (seq #,(racketidfont ".") term (sub1 n))
				 (term (sub1 n)))])))]
        (s%)))))



@section[#:tag "Exercises (Ch 8.)"]{Exercises}

@subsection{Functional programming with objects}

@margin-note{@secref{soln04}}

One perspective that unifies the paradigms of programming with
functions and programming with objects is to view a ``function'' as an
object that understands a method called @r[apply].  With that in
mind, we can define an interface for functions-as-objects:

@#reader scribble/comment-reader
(racketblock
;; A [IFun X Y] implements:
;; apply : X -> Y
;; Apply this function to the given input.
)

Here we are representing a @tt{X -> Y} function as an object that
has an @r[apply] method that consumes an @tt{X} and produces a
@tt{Y}.

@itemlist[

@item{Design a class that wraps a real function with contract @tt{(X
-> Y)} to implement @tt{[IFun X Y]}.}

@item{Using your wrapper class, construct the objects representing the
  ``add 1'' function and ``sub 1'' function.}

@item{Another useful operation on functions is composition.  Here is
  the interface for a @r[compose] method that composes two functions
  represented as objects:

@#reader scribble/comment-reader
(racketblock
;; [IFun X Y] also implements:
;; compose : [IFun Y Z] -> [IFun X Z]
;; Produce a function that applies this function to its input, 
;; then applies the given function to that result.
)

For example, if @r[addone] and @r[subone] refer to the objects
you constructed in part 2, the following check should succeed:

@#reader scribble/comment-reader
(racketblock
(check-expect ((addone #,dot compose subone) #,dot apply 5) 5)
(check-expect ((addone #,dot compose addone) #,dot apply 5) 7)
)

Implement the @r[compose] method for your wrapper class.
}
]
