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

@title[#:tag "lec10"]{2/10: Being Smart}


@section[#:tag-prefix "lec10"]{Announcements}


@itemlist[#:style 'ordered

@item{There will be no practice exam.}
@item{PDF Course notes are posted.}
@item{Next assignment due in 2 weeks.}
@item{The exam is in 110, not 108.}
@item{Study for the exam.}]

@section{The 3-In-A-Row Game}

@section{Indexed Views on Data}

@section{State Transition Patterns}

@section{Designing a Computer Player}

@subsection{A simple player}
@subsection{Evaluating positions}
@subsection{Picking a move}
@subsection{Remembering the past}