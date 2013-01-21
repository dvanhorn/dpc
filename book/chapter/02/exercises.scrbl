#lang scribble/manual
@(require class/utils)

@title[#:tag "Exercises (Ch. 2)"]{Exercises}

@section{Lists of Numbers}

@margin-note{@secref{Lists_of_Numbers_solution}}

Design classes to represent lists of numbers.  Implement the methods
@racket[length], @racket[append], @racket[sum], @racket[prod],
@racket[contains?], @racket[reverse], @racket[map], and @racket[max].
Note that @racket[max] raises some interesting design decisions in the
case of the empty list.  One solution is to define the @racket[max] of
the empty list as negative infinity, @racket[-inf.0], a number smaller
than every other number (except itself).  Another solution is to only
define @racket[max] for non-empty lists of numbers.

@section{Home on the Range}

@margin-note{@secref{Home_on_the_Range_solution}}

A @emph{range} represents a set of numbers between two endpoints.  To
start with, you only need to consider ranges that @emph{include} the
smaller endpoint and @emph{exclude} the larger endpoint---such ranges
are called @emph{half-open}.  For example, the range [3,4.7) includes
all of the numbers between 3 and 4.7, including 3 but @emph{not}
including 4.7. So 4 and 3.0000001 are both in the range, but 5 is
not.  In the notation used here, the ``['' means include, and the ``)''
means exclude.


@itemlist[

@item{Design a representation for ranges and implement the
  @r[in-range?] method, which determines if a number is in the
  range.  For example, the range [3,7.2) includes the numbers 3
  and 5.0137, but not the numbers -17 or 7.2.}

@item{Extend the data definition and implementation of ranges to
  represent ranges that @emph{exclude} the low end of the range and
  @emph{include} the high end, written (lo,hi].}

@item{Add a @r[union] method to the interface for ranges and implement
  it in all range classes.  This method should consume a range and
  produces a new range that includes all the numbers in this range
  @emph{and} all the numbers in the given range.

  You may extend your data definition for ranges to
  support this method.
  
  Don't worry if your initial design duplicates code; you can abstract
  later.
}
]

