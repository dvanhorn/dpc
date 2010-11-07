#lang scribble/manual
@(require scribble/eval)

@(require (for-label lang/htdp-intermediate-lambda))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval `(require (only-in lang/htdp-intermediate-lambda sqr)))
    (the-eval `(require test-engine/racket-tests))
    ;(the-eval `(require lang/htdp-intermediate-lambda))
    the-eval))

@title{@bold{Fundamentals of Computer Science II (Honors)}
       @linebreak[]
       Introduction to Class-based Program Design}

@larger{@bold{Spring, 2011}}

The course studies the class-based program design and the design
of abstractions that support the design of reusable software and
libraries. It covers the principles of object oriented program 
design, the basic rules of program evaluation, and examines the 
relationship between algorithms and data structures, as well as 
basic techniques for analyzing algorithm complexity.

The course is suitable for both CS majors and non-majors.  It 
assumes that student has been introduced to the basic principles 
of program design and computation. 

@bold{Prerequisites}

The course assumes proficiency with the systematic design of 
programs and some mathematical maturity. It demands curiosity 
and self-driven exploration and requires a serious commitment 
to practical hands-on programming. 

@include-section["general.scrbl"]

@include-section["texts.scrbl"]

@include-section["syllabus.scrbl"]

@include-section["lectures.scrbl"]

@include-section["labs.scrbl"]

@include-section["assignments.scrbl"]

@section{Blog}

@subsection{Welcome to CS2510H.}
              