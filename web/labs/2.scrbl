#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          "../unnumbered.rkt"
          "../utils.rkt"
          (for-label (except-in class0 define-struct)
                     2htdp/image
                     (only-in lang/htdp-intermediate-lambda define-struct)
                     class0/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class0))
    (the-eval '(require 2htdp/image))
    the-eval))

@title[#:tag "lab02"]{1/17: MLK (No lab)}

There is no lab on 1/17 in observation of Martin Luther King, Jr. Day.