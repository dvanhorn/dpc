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

@title[#:tag "lec14"]{2/24: Visitors}

@emph{This outline will be fleshed out soon}

@verbatim|{

Number generation game

  Want to remember and avoid generating numbers rejected by player
    Store in generator

  Why does `pick' terminate?
    You could stay unlucky forever...
      We'll avoid the delicious subtleties of this issue and move on

  sudo read xkcd

  With mutation
    Q: Is mutation bad design?
      Functional design is easier to test
      But mutation can sometimes simplify our programs

  Testing randomness is hard
  Testing mutation is hard

  Question Dan's attitude
  Question Dan's entropy

  Change `tell-bad : Number ->' to `tell-bad : ->'

  Property testing: as the number of inputs goes down, the amount of state
  stored in the generator goes up, along with the complexity of the test

  How do we apply the number generator game to the homework?

Visitor pattern

  You already know it...

  Add one general method to your classes, `visit'
  Support many externally defined operations
  Robust against changes in representation
    e.g. 3union-range

  ...it's a fold

  Folds and visitors apply to more than just lists, and more than just
  functional data

}|
