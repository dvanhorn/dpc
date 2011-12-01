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

@title[#:tag "lec19"]{3/14: Equality}

@section[#:tag "lec19-announce"]{Announcements}

@itemlist[
@item{Exam 2 is in 8 days, 3/22.  Room to be announced soon.}
@item{Next homework will be out tonight, due on Wed 3/23.}
@item{We will switch homework partners today, send your preferences by 5 pm.}
]

@section{Equality}

@subsection{Several kinds of equality}
@itemlist[
@item{Structural equality---identical twins are the "same".}
@item{Intensional equality---are we pointing at the same thing?}
]
Distinction matters a lot in the context of mutation.  If you punch
one identical twin, the other one doesn't get a black eye. 

@section{Defining structural equality}

@codeblock{
#lang class4
;; A Posn is a (posn% Number Number)
(define-class posn%
  (fields x y)

  ;; =? : Posn -> Bool
  ;; is the given posn the same as this one?
  (define/public (=? p)
    (and (= (field x) (send p x))
	 (= (field y) (send p y))))

  (check-expect (send (posn% 3 4) =? (posn% 3 4)) true)
  (check-expect (send (posn% 3 4) =? (posn% 3 5)) false)
)

}

What about lists?

@codeblock{
#lang class3
;; A LoP is one of either
;; - (mt%)
;; - (cons% Posn LoP)
(define-class mt%)
(define-class cons%
  (fields first rest))
}

What does it mean for the elements have to be the same?

In @r[mt%]:

@codeblock{
;; LoP -> Boolean 
(define/public (=? lop)
  (send lop empty?))
}

So we need to add @r[empty?] to our interface definition.
@codeblock{
;; A LoP is one of either
;; - (mt%)
;; - (cons% Posn LoP)
;; and implements:
;; empty? : -> Boolean
;; is this LoP empty? 
}

In @r[mt%]:
@codeblock{
(define/public (empty?) true)
}

In @r[cons%]:
@codeblock{
(define/public (empty?) false)
}

Now to implement equality for @r[cons%]:

@codeblock{
(define/public (=? lop)
  (and (not (send lop empty?)
	    (send (field first) =? (send lop first))
	    (send (field rest) =? (send lop rest)))))
}

Note that we're using the @r[=?] method to compare the @r[Posn]s in
the list. 

@section{Multiple Representations}

@codeblock{
;; A Posn implements IPosn

;; An IPosn implements
;; x : -> Number
;; y : -> Number
  ;; =? : IPosn -> Bool
  ;; is the given posn the same as this one?

;; A Posn2 is a (posn2% Number Number)
(define-class posn2%
  (fields ls)
  (constructor (x y)
	       (fields (list x y)))
  (define/public (x) (first (field ls)))
  (define/public (y) (second (field ls)))
  (define/public (=? p)
    (and (= (x) (send p x))
	 (= (y) (send p y)))))
}

Now we can compare the two different kinds of posns and everything
works properly.

What about multiple representations for lists of posns?


@codeblock{
#lang class3

;; A Posn implements IPosn.

;; An IPosn implements:
;;
;; x : -> Number
;; y : -> Number
;; =? : IPosn -> Boolean

;; A Posn2 is a (posn2% Number Number).
;; implement IPosn.
(define-class posn2%
 (fields ls)
 (constructor (x0 y0)
   (fields (list x0 y0)))

 (define/public (x)
   (first (field ls)))

 (define/public (y)
   (second (field ls)))

 (check-expect (send (posn2% 3 4) =? (posn2% 3 4)) true)
 (check-expect (send (posn2% 3 4) =? (posn2% 3 5)) false)
 (define/public (=? p)
   (and (= (send this x) (send p x))
        (= (send this y) (send p y)))))


(check-expect (send (posn% 1 2) =? (posn2% 1 2)) true)
(check-expect (send (posn% 1 2) =? (posn2% 1 1)) false)


;; A Posn1 is a (posn% Number Number).
;; implements IPosn.
(define-class posn%
 (fields x y)


 ;; Posn -> Boolean
 ;; Is the given posn the same as this?
 (check-expect (send (posn% 3 4) =? (posn% 3 4)) true)
 (check-expect (send (posn% 3 4) =? (posn% 3 5)) false)
 (define/public (=? p)
   (and (equal? (field x) (send p x))
        (equal? (field y) (send p y)))))

;; A LoP is one of:
;; - (mt%)
;; - (cons% Posn LoP)
;; implements ILoP

;; An ILoP implements:
;;
;; first : -> Posn
;; rest : -> ILoP
;; empty? : -> Boolean

(define the-real-empty? empty?)
(define the-real-first first)
(define the-real-rest rest)

(define-class list%
 (fields ls)
 (define/public (empty?)
   (the-real-empty? (field ls)))

 (define/public (first)
   (the-real-first (field ls)))

 (define/public (rest)
   (list% (the-real-rest (field ls))))

 (define/public (=? lop)
   (cond [(send this empty?) (send lop empty?)]
         [else
          (and (not (send lop empty?))
               (send (send this first) =? (send lop first))
               (send (send this rest) =? (send lop rest)))])))








;; A [Listof X] is one of:
;; - (mt%)
;; - (cons% X [Listof X])
;; where X implements
;; =? : X -> Boolean

(define-class mt%

 ;; [Listof X] -> Boolean
 ;; Is the given list of posns the same as this?
 (define/public (=? lop)
   (send lop empty?))

 (define/public (empty?)
   true))

(define-class cons%
 (fields first rest)

 (define/public (=? lop)
   (and (not (send lop empty?))
        (send (field first) =? (send lop first))
        (send (field rest) =? (send lop rest))))

 (define/public (empty?)
   false))

(check-expect (send (mt%) =? (mt%)) true)
(check-expect (send (cons% (posn% 3 4) (mt%)) =? (cons% (posn% 3 4) (mt%)))
             true)
(check-expect (send (cons% (posn% 3 4) (mt%)) =?  (mt%))
             false)
(check-expect (send (cons% (posn% 3 4) (mt%)) =? (cons% (posn% 3 5) (mt%)))
             false)

(check-expect (send (list% empty) =? (list% empty)) true)
(check-expect (send (list% (list (posn% 3 4))) =? (list% (list (posn% 3 4))))
             true)
(check-expect (send (list% (list (posn% 3 4))) =? (list% (list)))
             false)
(check-expect (send (list% (list (posn% 3 4))) =? (list% (list (posn% 3 5))))
             false)

(check-expect (send (list% (list (posn% 3 4))) =? (cons% (posn2% 3 4) (mt%)))
             true)

}

Now we can make all of the appropriate combinations work together:
different kinds of lists with the same kind of posns, the same kind of
lists with different kinds of posns, and different kinds of lists with
different kinds of posns.

@section{Intensional equality}

How can we tell if two posns are two different names for the same
thing, or if they're two different posns with the same contents?  

For example:

@codeblock{
(define p1 (posn% 3 4))
(define p2 (posn% 3 4))
}

or 

@codeblock{
(define p1 (posn% 3 4))
(define p2 p1)
}

These are very different in the presence of mutation.  We have a way
of testing this: @r[eq?].  Similarly, the @r[equal?] function checks
structural equality.  

But that didn't shed any light on the question.  Is there a way we can
check this @emph{in} our language?

Answer: yes.  Do some operation to one of them that changes the state
of the object, and see if it @emph{also} happens to the other one.

Drawback: you can't necessarily undo this operation.  

@codeblock{
(define (really-the-same? p1 p2)
  ....)
}

Now we need some operation to perform on @r[p1].  

@filebox[@tt{posn%}]{
@codeblock{
;; -> (posn% 'black-eye Number)
(define/public (punch!)
  (begin
    (set-field! x 'black-eye)
    this))
}
}

@racketblock[
(define sam (posn% 3 4))
(send sam punch!)
]

"I punched him so hard, I punched him right out of the data
defintion."

Now we can define @r[really-the-same?].

@codeblock{
(define (really-the-same? p1 p2)
  (begin
    (send p1 punch!)
    (symbol? (send p2 x))))
}

@racketblock[
(really-the-same? p1 p2)
p1
]

Now @r[p1] is permanently broken, and can't be undone.  So
@r[really-the-same?] is a very problematic, and you should use
@r[eq?], which uses DrRacket's internal knowledge of where things came
from to answer this question without changing the objects.  

Question:

@racketblock[
(eq? p1 (send p1 punch!))]

Produces true.

@racketblock[
(send p2 =? (send p2 punch!))
]

Produces true (or crashes).


@racketblock[
(send (send p3 punch!) =? p3)
]

Produces true (or crashes).

Question:
Does intensional equality imply structural equality?  Yes.


@section{Parameterized Data Defintions and Equality}

Generalizing @tt{LoP} to @tt{[Listof X]}.


@codeblock{
;; A [Listof X] is one of:
;; - (mt%)
;; - (cons% X [Listof X])
}

But we have to change one more thing.  Our @r[=?] method assumes that
@r[X] implements an @r[=?] method themselves.

@codeblock{
;; A [Listof X] is one of:
;; - (mt%)
;; - (cons% X [Listof X])
;; where X implements
;; =? : X -> Boolean
}

We could also change the signature of @r[=?] to take a comparison.
But then we'd have to change all of our code.  We've lifted a
restriction, but only to things that can be compared for equality.  

@section{Equality in Java}

@verbatim|{

class Posn {
   Integer x;
   Integer y;
   Posn(Integer x, Integer y) {
       this.x = x;
       this.y = y;
   }

   public Boolean isEqual(Posn p) {
       return this.x == p.x
           && this.y == p.y;
   }
}

interface LoP {
   Boolean isEmpty();
   Posn getFirst();
   LoP getRest();
}

class MT implements LoP {
   MT() {}

   public Posn getFirst() {
       return null;
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


