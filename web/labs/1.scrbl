#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          "../unnumbered.rkt"
          "../utils.rkt"
          (for-label class0
                     2htdp/image
                     2htdp/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require lang/htdp-intermediate-lambda))
    (the-eval '(require class0))
    (the-eval '(require 2htdp/image))
    (call-in-sandbox-context
     the-eval
     (lambda () ((dynamic-require 'htdp/bsl/runtime 'configure)
                 (dynamic-require 'htdp/isl/lang/reader 'options))))
    the-eval))

@title[#:tag "lab01"]{1/10: Introduction to objects}

@internal{

@bold{TODO:}
@itemlist[
  @item{#lang or ASL?}
  @item{style for calling methods on @racket[this]?}
  @item{@racket[big-bang] or World objects?}
  @item{named args -> positional args}
]

@section{Partner assignments}

@section{Getting set up}

@subsection{svn basics, submit assn0}

@subsection{Install teachpacks}

@section{Playing with objects}

Here is a simple class in Racket:

@#reader scribble/comment-reader
(racketblock+eval #:eval the-eval
  ; A Ball is a (new ball% Number Number Number Color)
  (define-class ball%
    (fields x y radius color)

    ; Ball -> Boolean
    ; Do the Balls have the same color?
    (define/public (same-color? b)
      (equal? (field color) (send b color)))

    ; -> Image
    ; The image representing the Ball
    (define/public (draw)
      (circle (field radius) "solid" (field color))))
)

Recall that we can create and use @racket[Ball] objects as follows:

@interaction[#:eval the-eval
  (define b (new ball% 50 25 10 "red"))
  b
  (send b x)
  (send b radius)
  (send b same-color? (new ball% 10 20 3 "red"))
]

@exercise{
  Write the method @racket[diameter] for the class @racket[ball%] that
  calculates the Ball's diameter.
}

@exercise{
  Write the method @racket[distance-to] for the class @racket[ball%] that
  takes a Posn and returns the distance from the center of the Ball to the Posn.
}

@exercise{
  Write the method @racket[overlaps?] for the class @racket[ball%] that takes
  another Ball and determines whether the two Balls overlap.
}

@exercise{
  Write a class @racket[block%] to represent rectangular blocks on the screen.
  Include enough fields to record its position, size, and color. Construct
  some examples of Blocks.

  Write a @racket[draw] method for @racket[block%] that returns an image
  representing the Block.
}

}
