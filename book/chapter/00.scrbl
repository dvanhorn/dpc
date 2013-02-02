#lang scribble/manual
@(require scribble/eval
          scriblib/figure
          scriblib/footnote
          class/utils)

@title[#:style '(grouper unnumbered)]{Preface}

This book is a @bold{draft} textbook to accompany Fundamentals of
Computer Science II (Honors): Introduction to Class-Based Program
Design at Northeastern
University.@note{@url{http://www.ccs.neu.edu/course/cs2510h/}} It
began its life as a series of course notes from the Spring 2011
incarnation of the class, evolved during Spring 2012, and is now being
revised throughout the Spring 2013 semester.

The ``book'' is very much a work in progress, which means there are
large omissions and numerous errors.  Moreover, it's certain to change
as the course progresses.  Your patience, feedback, and bug reports
are greatly appreciated.  The book and its accompanying software are
maintained on @link["http://github.com/"]{@as-index{Github}} at the following
URL:

@centered{@url{https://github.com/dvanhorn/dpc}}



@section[#:style 'unnumbered]{Design Recipes}

@indented{@emph{
It puts a lot of emphasis on something called The Design Recipe, which
sounded hokey to me at first, but when I was shown what The Design
Recipe was, I realized, even as someone who's been programming for 35
years, that rather than code a solution to something complex in say,
an hour, after 15-20 minutes, when I thought I was a third done, I was
actually and suddenly completely done, sometimes almost feeling like
magic. It basically puts a high emphasis on thinking and designing
first, writing test cases very early, picking the right program
structure, and then filling in the blanks and expanding them in a few
places. Following the Design Recipe, not only was I often ``suddenly
done,'' but because my tests were written before I started coding the
meat of a solution, I had (and continue to have) high confidence that
my program works and will continue to work (or alert me quickly to new
errors) should I make any ``enhancements'' down the road.}

--- @index["Knauth, Geoffrey S."]{Geoffrey S. Knauth}, 2011
}

The main focus of this book is the @emph{design process} that leads
from problem statements to well-organized solutions, oriented around
the concept of @emph{objects}.  We make extensive use of explicit
design guidelines formulated as a number @emph{program design recipes}
as described in @link["http://htdp.org/"]{@as-index{@emph{How to Design
Programs}}}.  The most general form of the recipe for designing
programs is given in @figure-ref{design-recipe}.

@figure-here["design-recipe" "The Design Recipe for Programs"]{
@itemlist[#:style 'ordered
@item{Problem Analysis & Data Definition}
@item{Contract, Purpose & Effect Statements, Header}
@item{Examples}
@item{Template}
@item{Code}
@item{Tests}]}

The objective of the design recipe is to provide a @deftech{technique}
that guides programmers---novices and professionals alike---with a
systematic approach to problem solving.  By reasoning systematically,
programmers can eliminate the incidental complexities of writing
software and instead focus their creative energy on what is
essentially difficult about a particular problem.  A great programmer
is first a master technician; they continue to hone and internalize
their skills with each day of programming.  As the chef @index["Pépin,
Jacques"]{Jacques Pépin} said, @note{@as-index{@emph{New York Times}},
``@link["http://www.ccs.neu.edu/home/dvanhorn/tmp/programming-is-like-cooking.pdf"]{There's
the Wrong Way and Jacques Pépin's Way}'' October 18, 2011.} speaking
of another field that values the idea of recipes, to master the
technique, you have no choice: ``you have to repeat, repeat, repeat,
repeat until it becomes part of yourself.''


@;Each steps produces a well-defined intermediate product


@section[#:style 'unnumbered]{The Choice of Language and Environment}

@indented{@emph{Another lesson we should have learned from the recent
past is that the development of ``richer'' or ``more powerful''
programming languages was a mistake in the sense that these baroque
monstrosities, these conglomerations of idiosyncrasies, are really
unmanageable, both mechanically and mentally. I see a great future for
very systematic and very modest programming languages.}

--- @index["Dijkstra, Edsger W."]{Edsger W. Dijkstra}
``@link["http://www.cs.utexas.edu/users/EWD/transcriptions/EWD03xx/EWD340.html"]{The
Humble Programmer}'', @as-index{Turing Award} Lecture, 1972}

In support of this book, we have a developed a series of modest
programming languages that emphasize the principles of object-oriented
design.  We use these languages throughout the first part of the book.
They are deliberately not industrial strength programming languages,
complete with full-featured libraries, convenience mechanisms, and the
usual idiosyncrasies that accompany ``real'' languages.  Instead we
have designed a progression of simple and consistent languages which
embody the common core of modern object-oriented languages.  The idea
is that by focusing on the conceptual basis of object-oriented
programming, students can apply their design knowledge regardless of
whatever linguistic context they happen to find themselves in down the
road.

In the second part, we transition to @as-index{Java}, a widely used
industrial language that has been developed over more than 15 years.
The move to Java allows us to explore the application of the
principles introduced in the first portion of the course in the
context of a practical language.

As of this draft of the book, the course software has been developed
and tested with version 5.2 of
@link["http://racket-lang.org/"]{@as-index{Racket}}.  To install the course
software, you will first need to install Racket:

@centered{@url{http://racket-lang.org/}}

Once installed, launch @as-index{DrRacket}, the development
environment that ships with the Racket system.  Click on the
@menuitem["File" "Install PLT File"], and then enter the URL:

@centered{@url{http://www.ccs.neu.edu/course/cs2510h/class-system-latest.plt}}

After the PLT file has been installed, select @menuitem["Language"
"Choose Language..."] and select the ``Use the language declared in the
source'' option.  You can now write programs in any of the languages
included in the course software by writing @tt{#lang class/@emph{N}}
as the first line of a file, where @emph{N} is a number in @tt{0},
@tt{1}, @tt{2}, etc.  To find out more about the languages, use Help
Desk and search for @tt{class/0} to get started.


@section[#:style 'unnumbered]{The Parts of the Book}

@section[#:style 'unnumbered]{Acknowledgments}

We are grateful to @index["Felleisen, Matthias"]{Matthias Felleisen},
our TAs: @index["Brown, Daniel"]{Daniel Brown} (2011),
@index["Takikawa, Asumu"]{Asumu Takikawa} (2012), and @index["Labich,
Nicholas"]{Nicholas Labich}(2013), our tutors: @index["Lee,
Alex"]{Alex Lee}, @index["Patten, Nikko"]{Nikko Patten},
@index["Shargo, Jim"]{Jim Shargo}, @index["Sontag, Trevor"]{Trevor
Sontag} (2011), @index["Florence, Spencer"]{Spencer Florence},
@index["Laplante, Sarah"]{Sarah Laplante}, @index["Plessner,
Ryan"]{Ryan Plessner} (2012), @index["MacKenzie, Becca"]{Becca
MacKenzie}, and @index["Mullins, Kathleen"]{Kathleen Mullins} (2013),
and the Northeastern students we have had the privilege of teaching in
2011 and 2012.

