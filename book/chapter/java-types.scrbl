#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/4 check-expect define-struct ... length
                                numerator denominator))
          (for-label 2htdp/image)
          (for-label (only-in test-engine/racket-tests check-expect))
          (for-label class/universe))

@section{Types}

What sorts of things count as @emph{types} in Java? 

@itemlist[
@item{Class names}
@item{Interface names}
@item{Other stuff: @tt{int}, @tt{boolean}, ...}
]

What should be a part of the contract and purpose in Java? Well, we
don't need to write down things that are already part of the type.  If
the contract corresponds to the type, you don't have to repeat it.
However, some contracts can't be checked by the type system---you
should still write those down.
