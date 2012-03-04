#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/1 define-struct ... length first rest
                                cons empty append reverse map filter foldl foldr))
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

@(the-eval 
  '(begin
     (define-class empty%
       (define (map f)
	 this))
     (define-class cons%
       (fields first rest)
       (define (map f) 
	 (new cons% 
	      (f (send this first))
	      (send (send this rest) map f))))))

@title{Parameterized Data and Interfaces}

@section{Parametric data}

Consider the parametric data definition for lists we studied last
semester:

@classblock{
;; A [List X] is one of:
;; - empty
;; - (cons X [List X])
}

Recall this is really not just a single data definition, but a family
of data definitions.  We can obtain a different member of this family
by plugging in some data definition for @tt{X}; @tt{List X} works
just like a function---apply it to arguments results in its
definition, but with @tt{X} replaced by the argument.  Unlike with a
function, the arguments are not values, but @emph{data definitions}.
So for example, plugging in @tt{Number} for @tt{X}, written
@tt{[List Number]} results in:

@classblock{
;; A [List Number] is one of:
;; - empty
;; - (cons Number [List Number])
}

Applying @tt{List} to @tt{String} results in:

@classblock{
;; A [List String] is one of:
;; - empty
;; - (cons String [List String])
}

The process by which we obtained the parameterized data definition,
@tt{List X}, is one of abstraction.  Looking back, we
@emph{started} with the non-parameterized data definitions:

@classblock{
;; A ListNumber is one of:
;; - empty
;; - (cons Number ListNumber)

;; A ListString is one of:
;; - empty
;; - (cons String ListString)
}

These definitions are so similar, it's natural to want to abstract
them to avoid repreating this nearly identical code again and again.
It's easy to see what is different: @tt{Number} vs @tt{String}, and
that's exactly how we arrived at @tt{[List X]}.

We can repeat the process for objects.  Let's start with the
non-parameterized data definitions we've been working worth:

@classblock{
;; A ListNumber is one of:
;; - (new emtpty%)
;; - (new cons% Number ListNumber)

;; A ListString is one of:
;; - (new empty%)
;; - (new cons% String ListString)

(define-class empty%)
(define-class cons%
  (fields first rest))
}

Abstracting out the data definition of elements results in:

@classblock{
;; A [List X] is one of:
;; - (new empty%)
;; - (new cons% X [List X])
}

@section{Parametric interfaces}

We have now developed a parametric data definition for lists, focusing
on the representation of lists.  If instead we focused the bevahiors
of lists, we would arrive at a @emph{parametric interface}:

@classblock{
;; A [List X] implements:
;; - cons : X -> [List X]
;;   Cons given value on to this list.
;; - first : -> X 
;;   Get first element of this non-empty list.
;; - rest : -> [List X]
;;   Get the reset of this non-empty list.
}

We can implement the interface as follows:

@classblock{
;; A (new empty%) implements [List X]
(define-class empty%
  (define (cons x) (new cons% x this)))

;; A (new cons% X [List X]) implements [List X]
(define-class cons%
  (fields first rest)
  (define (cons x) (new cons% x this)))
}

@section{Parameteric methods}

We can design further methods:

@classblock{
;; A [List X] implements:
;; - cons : X -> [List X]
;;   Cons given value on to this list.
;; - first : -> X 
;;   Get first element of this non-empty list.
;; - rest : -> [List X]
;;   Get the reset of this non-empty list.
;; - length : -> Natural
;;   Get the length of this list.
;; - append : [List X] -> [List X]
;;   Append this to given list.
;; - reverse : -> [List X]
;;   Reverse this list of elements.
;; - map : [X -> Y] -> [List Y]
;;   Map given function over this list.
;; - filter : [X -> Boolean] -> [List X]
;;   Select elements that satisfy the given predicate.
;; - foldr : [X Y -> Y] Y -> Y
;;   Fold right over this list.
;; - foldl : [X Y -> Y] Y -> Y
;;   Fold left over this list.
}

Again, the model of a parametric interface is as a @emph{function} of
classes of data.  In this interface, @tt{X} is the parameter, bound at
the point of ``@tt{A [List X] implements}''.  That variable occurs
several times within the definition and is replaced by the argument of
@tt{List}.  But on closer inspection, there are other variables that
are not bound, e.g. @tt{Y} in:

@classblock{
;; - map : [X -> Y] -> [List Y]
}

To be precise, there's really another, implicit, parameter that
doesn't range over the whole interface, but just the @racket[map]
method.  We can make this implicit parameter in the contract notation
by adding a class variable at the level of @racket[map]:

@classblock{
;; - map [Y] : [X -> Y] -> [List Y]
}

So for example, we might have a @tt{[List Number]}, in which case, the
object has the @racket[map] method:

@classblock{
;; - map [Y] : [Number -> Y] -> [List Y]
}

Even though the @tt{X} has been replaced by @tt{Number}, the @tt{Y}
parameter remains and only takes on a specific meaning from the
function argument of @racket[map].  So if @racket[ns] is a @tt{List
Number}, we apply map to a @tt{[Number -> String]} function, its
contract is interpreted with @tt{Y} replaced by @tt{String}:


@examples[#:eval the-eval
(define ns (new cons% 3 (new cons% 4 (new empty%))))
(eval:alts (ns #,dot map number->string) (send ns map number->string))]


@section{Exercises}

@subsection{Parametric lists}

Design an implementation of the @tt{[List X]} interface given in this
chapter.
