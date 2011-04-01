#lang scribble/manual

@title[#:tag "lec22"]{3/24: Exam review}

@section[#:tag "lec22-announce"]{Announcements}

@itemlist[
@item{Homework due tomorrow}
]


@section[#:tag-prefix "lec22"]{Problem 1}

You've just been hired by Adobe to write their next generatation PDF
rendering engine. As your first task, you’ve been assigned to write an
@emph{outline viewer} that displays the overall structure of a
document. A document can consist of any number of sections, and a
section has a title and any number of subsections, and a subsection
has a title and any number of subsubsection, and so on.

@itemlist[#:style 'ordered 

@item{Design a representation of documents
 that can handle documents like this:

@verbatim{
1. OO = struct + fun 
1.1. Rocket
1.2. Moon 
2. Design
2.1. Announce 
2.1.1. Assign 
3. End
}

Note that the section numbers are @emph{NOT} part of the document!

@codeblock{
;; A Sec is one of: 
;; - (b%) 
;; - (sec% String Sec Sec) 
;; A Doc is a Sec. 
(define-class b%) 
(define-class sec%
  (fields title sub next))
}
}

@item{Using your design, give the representation of the document in part 1.

@codeblock{
(sec% "OO = struct + fun"
      (sec% "Rocket" (b%)
             (sec% "Moon" (b%) (b%)))
      (sec% "Design"
             (sec% "Announce"
                   (sec% "Assign" (b%) (b%))
                   (b%))
            (sec% "End" (b%) (b%))))
}
}

@item{Design a method outline that produces a list of strings, which
are the sectional headings in order @emph{and with section numbers}.  

In other words, if @racket[d] represents the document in part 1,
@racket[outline] should produce the following:

@verbatim{
> (d . outline)
'("1. OO = struct + fun"
  "1.1. Rocket"
  "1.2. Moon"
  "2. Design"
  "2.1. Announce"
  "2.1.1. Assign"
  "3. End")
}

Let's attack the problem using @emph{iterative refinement}.  First,
let's forget about the section numbers and just try to produce a list
of headings:

@verbatim{
> (d . headings)
'("OO = struct + fun"
  "Rocket"
  "Moon"
  "Design"
  "Announce"
  "Assign"
  "End")
}

Solving this problem will help us see the solution to the over all
problem, and if nothing else, will get us partial credit.

This problem can be solved in a straightforward way using a
@emph{structurally recursive} design:

@filebox["b%"]{
@#reader scribble/comment-reader
(racketblock
  ;; -> [Listof String]
  ;; Headings in this blank section.
  (define/public (headings) empty)
)
}

@filebox["sec%"]{
@#reader scribble/comment-reader
(racketblock
  ;; -> [Listof String]
  ;; Headings in this non-blank section.
  (define/public (headings)
    (cons (field title)
          (append ((field sub) #,(racketidfont ".") headings)
                  ((field next) #,(racketidfont ".") headings))))
)
}

OK, so what is missing from this design?  We have not produced the
section numbers.  To make our lives easier, let's suppose somebody
@emph{tells us} which section number we are when asked to produce
the headings.  First let's make a helper function:

@codeblock{
;; Number -> String
;; Render a section number (5 => "5.").
(define (r n)
  (string-append (number->string n) "."))
}

Now we revise @racket[headings]:

@filebox["sec%"]{
@#reader scribble/comment-reader
(racketblock
  ;; Number -> [Listof String]
  ;; Headings with section number in this non-blank section.
  (define/public (headings n)
    (cons (string-append (r n) " " (field title))
          (append ((field sub) #,(racketidfont ".") headings ??)
                  ((field next) #,(racketidfont ".") headings ??))))
)
}

What should we do for the missing arguments to the recursive calls to
@racket[headings]?  In the case of @racket[next], the answer is clear:
the @racket[next] section should be number @racket[(add1 n)].  But for
@racket[sub]?  We really want to start over, numbering the subsections
from @racket[1].  So we get the following:


@filebox["sec%"]{
@#reader scribble/comment-reader
(racketblock
  ;; Number -> [Listof String]
  ;; Headings with section number in this non-blank section.
  (define/public (headings n)
    (cons (string-append (r n) " " (field title))
          (append ((field sub) #,(racketidfont ".") headings 1)
                  ((field next) #,(racketidfont ".") headings (add1 n)))))
)
}

Now what's missing from this design?  When we produce the headings for
subsections, they are missing the section number for the current
section.  In other words, we will see results like this:

@verbatim{
> (d . headings)
'("1. OO = struct + fun"
  "1. Rocket"
  "2. Moon"
  "2. Design"
  "1. Announce"
  "1. Assign"
  "3. End")
}

Well, that's easy to fix: just add the current section number to 
each heading of subsections:

@filebox["sec%"]{
@#reader scribble/comment-reader
(racketblock
  ;; Number -> [Listof String]
  ;; Headings with section number in this non-blank section.
  (define/public (headings n)
    (cons (string-append (r n) " " (field title))
          (append (map (λ (h) (string-append (r n) h))
                       ((field sub) #,(racketidfont ".") headings 1))
                  ((field next) #,(racketidfont ".") headings (add1 n)))))
)
}

Now we have a complete solution by having the @racket[outline] method
simply defer to @racket[heading] with the number @racket[1].

@filebox["b%"]{
@#reader scribble/comment-reader
(racketblock
  ;; -> [Listof String]
  ;; Outline for this section.
  (define/public (outline)
    (headings 1))

  ;; Number -> [Listof String]
  ;; Headings with section number in this blank section.
  (define/public (headings n) empty)
)
}

@filebox["sec%"]{
@#reader scribble/comment-reader
(racketblock
  ;; -> [Listof String]
  ;; Outline for this section.
  (define/public (outline)
    (headings 1))

  ;; Number -> [Listof String]
  ;; Headings with section number in this non-blank section.
  (define/public (headings n)
    (cons (string-append (r n) " " (field title))
          (append (map (λ (h) (string-append (r n) h))
                       ((field sub) #,(racketidfont ".") headings 1))
                  ((field next) #,(racketidfont ".") headings (add1 n)))))
)
}


@verbatim{
(check-expect ((b%) . outline) empty)

(check-expect ((sec% "Exam" (sec% "Aced" (b%) (b%)) (b%)) . outline)
              '("1. Exam" "1.1. Aced"))

(check-expect ((sec% "Exam" (b%) (sec% "Aced" (b%) (b%))) . outline)
              '("1. Exam" "2. Aced"))
}


}

]

@section[#:tag-prefix "lec22"]{Problem 2}

The Programming Research Lab has been acquiring some new decorative
objects, in the form of rectangles and rectangular solids.
Unfortunately, they've all come unpainted, and now the graduate
students have to paint them.  Dan has been trying to calculate how
much this will cost, and he's enlisted you to help.  

First, some basic facts.  Rectangles have an area which is the width
times height, and must be painted on both sides.  Rectangular solids
have a surface area which is the sum of the areas of all of the
sides, or @emph{2 x (w x d + d x h + w x h)}. Red paint costs 2
dollars per square foot, and blue paint costs 5 dollars per square
foot.  
%
Each decorative object has a color, and the appropriate dimensions,
measured as an integer number of feet.  

Design a representation for these decorative objects, and implement
the @racket{expense} method.  You should use @emph{overriding} to avoid
repeating code---in particular, you should only have one version of
the @racket{expense} method.




@section[#:tag-prefix "lec22"]{Problem 3}

@section[#:tag-prefix "lec22"]{Problem 4}

@section[#:tag-prefix "lec22"]{Problem 5} 

