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

@title[#:tag "lab03"]{1/24: Interfaces and inheritance}

@internal[

@itemlist[
  @item{New class system}
  @item{Inheritance: abstraction, reuse}
  @item{Interfaces: separation of concerns, modularity, swappable
        implementations}
  @item{Give them a client of an interface, have them provide multiple
        implementations}
]

]
