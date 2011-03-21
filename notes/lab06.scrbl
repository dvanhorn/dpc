#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          "../web/lab.rkt"
          "../web/unnumbered.rkt"
          "../web/utils.rkt"
          (for-label (except-in class2 define-struct)
                     2htdp/image
                     (only-in lang/htdp-intermediate-lambda define-struct)
                     class1/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class2))
    ;(the-eval '(require 2htdp/image))
    ;(the-eval '(require class1/universe))
    the-eval))

@(define exercise (exercise-counter))

@title[#:tag "lab06"]{2/07: Mutation and cyclic data}

@lab:section{Meditate}

@exercise{
  What are the values of @racket[a], @racket[b], and @racket[c] at each marked
  point below? Try to predict the behavior of each before testing it in
  DrRacket.

  @#reader scribble/comment-reader
  (racketblock
  (define-class box%
    (field x)

    (define/public (take new-x)
      (set-field! x new-x))

    (define/public (with new-x)
      (box% new-x)))

  (define a 0)
  (define b (box% a))
  (define c (box% b))

  ; a b c ?

  (b #,dot take 1)

  ; a b c ?

  (b #,dot with 2)

  ; a b c ?

  (c #,dot take (b #,dot with 3))

  ; a b c ?

  (c #,dot with (b #,dot take 4))

  ; a b c ?
  )

}

@lab:section{Lists}

Consider this familiar structure, but in an unfamiliar form:

@#reader scribble/comment-reader
(racketblock
; A [NonEmptyList X] is a (cons% X [List X])

; A [List X] is one of:
;  - (empty%)
;  - [NonEmptyList X]
)

@exercise{
  Define the classes @racket[empty%] and @racket[cons%].

  On @tt{List}s, define the methods @racket[empty?] and @racket[length].
}

@exercise{
  On @tt{NonEmptyList}s, define the effectful method @racket[insert-front!] that
  takes an element as input and inserts it at the front of the list. It should
  update the list in place and return nothing.
}

@exercise{
  On @tt{NonEmptyList}s, define the effectful method @racket[append!] that takes
  a @tt{List} as input and appends it to the end of the list. It should update
  the current list in place and return nothing.
}

@exercise{
  On @tt{NonEmptyList}s, define the effectful method @racket[insert-end!] that
  takes an element as input and inserts it at the end of the list. It should
  update the list in place and return nothing.
}

@exercise{
  On @tt{NonEmptyList}s, define the effectful method @racket[remove-front!] that
  removes the first element in the list and returns nothing.

  Assume the list has at least two elements.
}

@exercise{
  On @tt{NonEmptyList}s, define the effectful method @racket[remove!] that takes
  a natural number @racket[i] as input and removes the @racket[i]th element in
  the list and returns nothing.

  Assume that the list has at least @math{@racket[i]+1} elements.
}

@lab:section{Cyclic lists}

Now consider an unfamiliar structure:

@#reader scribble/comment-reader
(racketblock
; A [CList X] is a (clist% X [CList X] [CList X]) that 
(define-class clist%
  (fields data left right))
)

This is a representation of @emph{cyclic lists}---lists of data that allow you
to navigate both forward and backward along the links. Since there is no
``first'' element in the list, we will simply call those directions
@racket[left] and @racket[right].

@exercise{
  We don't want people carrying around invalid lists, so we will enforce that
  all constructed lists are valid.

  Define a constructor for @racket[clist%] that takes a single element as input
  and constructs a one-element cyclic list. If you need placeholder data, use
  something like @racket['invalid]---but make sure it doesn't escape your
  constructor!
}

@exercise{
  Define the method @racket[insert-right!] that takes an element as input and
  inserts it to the right of the current element in the list.
}

@exercise{
  Define the method @racket[insert-left!] that takes an element as input and
  inserts it to the left of the current element in the list.
}

Now you have enough methods to construct some example lists.

@exercise{
  Construct the cyclic list 0 → 1 → 2 → 0 → 1 → 2 → ⋯
}

@exercise{
  Construct the cyclic list 2 → 1 → 0 → 2 → 1 → 0 → ⋯
}

We can think of a cyclic list like a dial: we can access the current element,
and we can rotate it right or left to access data elsewhere. If we keep rotating
in the same direction, we will eventually come back to where we started.

@exercise{
  Define the methods @racket[rotate-left] and @racket[rotate-right] that take a
  natural number @racket[n] and returns the @tt{CList} to the left or right of
  us by @racket[n] steps.
}

@exercise{
  Define the method @racket[extract!] that extracts out the current element as
  its own singleton cyclic list. It should take no input, return the new
  singleton @tt{CList} containing the current element, and shrink the current
  list so it no longer contains the current element.
}

@exercise{
  Define the method @racket[join!] that takes a @tt{CList} named @racket[that]
  as input, mutates @racket[this] and @racket[that] so they form a single,
  bigger cyclic list, and returns nothing.
}
