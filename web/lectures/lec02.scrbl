#lang scribble/manual
@(require scribble/eval
	  "../utils.rkt"
          racket/sandbox
  	  (for-label (only-in lang/htdp-intermediate-lambda define-struct))
          (for-label (except-in class0 define-struct))
	  (for-label class0/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    ;(the-eval '(require lang/htdp-intermediate-lambda))
    (the-eval '(require class0))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@title[#:tag "lec02"]{1/13: Designing classes}

@internal{
@bold{Outline}
@itemlist[
 @item{Announcements
   @itemlist[
     @item{blah.}]}]

One of the most important lessons of @emph{How to Design Programs} is
that the structure of code follows the structure of the data it
operates on, which means that the structure of your code can be
derived @emph{systematically} from your data definitions.
}

