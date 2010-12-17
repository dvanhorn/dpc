#lang scribble/manual

@title[#:tag "style"]{The Style}

In addition to following the design recipe, all code written for this class
should adhere to the following basic style guidelines for your programs:

@itemlist[#:style 'ordered

@item{No line should span more than 80 characters. See bottom right of
DrRacket. Break lines at those points suggested by HtDP and this web page.}

@item{No function should span more than five to eight lines for
now. If it does, reconsider your interpretation of the "one task, one
function" guideline.}

@item{Use names that make sense with respect to the problem.}

@item{Start the file with globally useful data definitions and
constant definitions. Then arrange to place the most important
function near the top of the file and the less important ones near the
bottom. NOTE: You don't have to develop the functions in this order,
you just have to arrange the program this way.}

@item{Separate distinct sections of your program with dashed lines
that are exactly 80 columns wide.}

@item{Programs use the parentheses style displayed in HtDP and this
web page: 

@bold{Good}:

@verbatim{
(define (f l)
  (cond [(empty? l) 0]
        [else (add1 (f (rest l)))]))
}

@bold{Bad}:

@verbatim{
(define (f l)
  (cond [(empty? 1) 0]
        [else (add1 (f (rest l)))]
   )
  )
}

These dangling parentheses for the code in the right column are
considered @bold{extremely bad style}. You will lose @bold{all} style points for
using it even once.
}]

Not observing these very basic guidelines leads to unreadable code and
to loss of points.

