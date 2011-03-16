
#lang scribble/manual
@(require "unnumbered.rkt")

@(require (for-label class0))
@(require (for-label class0/universe))

@title*{Blog}

@section*{New homework partners}

@tt{Mon Mar 14 23:09:46 EDT 2011}

In effect immediately.  Let us know @bold{now} if there is a problem.

@verbatim{
pair 036: laplante, chiner
pair 037: mmitlin, fungi
pair 038: dcorbett, aldwin
pair 039: forrest, jccoates
pair 040: jgramm, wconlon
pair 041: lindeman, florence
pair 042: dmsilva, rpless
pair 043: tswartz, naraghi
pair 044: demaior, pfurtado
pair 045: mcarroll, robbyk
pair 046: ngoldman, petey3
pair 047: mcoppola, kardon
pair 048: wjhsie, ekelly
pair 049: gressen, maxgoren
pair 050: schwarta, fnimick
pair 051: gierb, hfried7, morehd
pair 052: markefus, romelusw
}



@section*{No Dan this week}

@tt{Sun Mar  6 16:35:35 EST 2011}

Dan will not hold office hours this week.

@section*{Assignment 6 pushed back}

@tt{Mon Feb 21 15:38:28 EST 2011}

We have decided to push back the due date on
@seclink["assign06"]{assignment 6} to Friday 2/25.  This will give us a
chance to talk more about the artificial intelligence design in class
on Thursday.  Please come prepared with questions.

In the meantime, we suggest you design an unintelligent connect four
player that always plays the first available move it can find.  This
will make your life easier as you try to design a smarter player.

@section*{How to Design Classes PDF released}

@tt{Mon Feb 21 15:37:20 EST 2011}

The authors of @emph{How to Design Classes} have decided to release
the draft version of the book as a PDF
@link["http://www.ccs.neu.edu/home/matthias/htdc.html"]{available on
the web}.


@section*{Exam Results}

@tt{Wed Feb 16 15:37:20 EST 2011}}

There were 84 possible points on the exam.

The average is 64.8, with a standard deviation of 13.7.

The quartiles are:

@verbatim{Low: 20
25%: 59.5
50%: 70
75%: 75
High: 83}

@section*{On the Exam}

@tt{Mon Feb 14 15:10:03 EST 2011}

The material from today's lecture on mutation will @emph{not} be
covered on tomorrow's exam.  All other topics from lectures, labs, and
homeworks are fair game.

@section*{Notes for Studying}

@tt{Wed Feb  9 19:31:26 EST 2011}

We've produced PDF versions of the course notes so far, as an aid to studying
for the upcoming exam.  A full page version is @link["notes.pdf"]{here} and a
shrunken version is @link["notes-2up.pdf"]{here}.  These are long documents, so
if you print them, please print them double-sided.  

@section*{Modules and homeworks}

@tt{Tue Feb  8 22:39:16 EST 2011}

You are free to use modules to structure your assignment solutions as
you see fit, so long as you have files @tt{1.rkt}, @tt{2.rkt}, etc.,
for each part of the assignment.

@section*{Exam 1 room: 110 WVH}

@tt{Mon Feb  7 21:35:25 EST 2011}

Exam 1 will take place in 110 WVH on Tuesday, 2/15/2011, from 6-PM.
Note that is @bold{NOT} where lecture is held (that's 108 WVH), but it
is right next door.  So if you mistakenly go to 108 and notice a bunch
of your peers going in to 110, you should probably follow them.

@section*{@tt{class2} Released}

@tt{Mon Feb  7 16:00:01 EST 2011}

A new version of the course software has been released, including
@tt{class2}.  The newest version of the class system is
@tt{class-system-02-07.plt}.

@section*{Course Software Fixed}

@tt{Thu Feb  3 17:06:01 EST 2011}

A new version of the course software has been released, fixing a
singificant bug in its use with DrRacket.  The newest version of the
class system is @tt{class-system-02-03.plt}.

@section*{Fixes to @racket[class1]}

@tt{Wed Feb  2 21:54:21 EST 2011}

Several bugs in the implementation of @racketmodname[class1], including the
use of @racketidfont["."] in the Interactions Window, have now been
fixed.  The newest version of the class system is
@tt{class-system-02-02.plt}.  

@section*{Subversion repository issue fixed}

@tt{Sun Jan 30 01:16:43 EST 2011}

There was an issue with the permissions set on the svn repository that
prevented people from accessing the new pair directories.  That issue
should be resolved, so let us know if you still cannot access your
pair's directory.


@section*{Second partnership assignments}

@tt{Thu Jan 27 17:12:44 EST 2011}

Here are the partner assignments for assignment 4 and on (until the
next partner assignment):

@verbatim{
pair 018: tswartz, aldwin
pair 019: ngoldman, schwarta
pair 020: chiner, wconlon
pair 021: laplante, ekelly
pair 022: maxgoren, fnimick
pair 023: rpless, jccoates
pair 024: demaior, florence
pair 025: gierb, markefus
pair 026: morehd, jgramm
pair 027: fungi, hfried7
pair 028: dcorbett, kardon
pair 029: lindeman, petey3
pair 030: mcoppola, robbyk
pair 031: forrest, dmsilva
pair 032: naraghi, mmitlin
pair 033: mcarroll, hloople
pair 034: pfurtado, gressen
pair 035: wjhsie, romelusw
}

@section*{Constructor-style printer}

@tt{Fri Jan 21 18:00:03 EST 2011}

We've released a new version of the course @tt{.plt} file that
implements a constructor-style printer for @racketmodname[class0] and
@racketmodname[class1].  This change makes object values print in the
same way they are constructed; it also fixes the issues we saw in
class with inherited fields printing in the wrong order.

Unfortunately, the tool we use for generating the web page doesn't use
this fancy printer, so for the time-being you'll have to live with the
discrepancy between what DrRacket prints and what's on the web.

@section*{Assignment 1 graded, assignment 3 tweaked}

@tt{Fri Jan 21 14:45:19 EST 2011}

There were only a couple remaining pairs that didn't have their
assignment 1 graded by Wednesday because one of our tutors was out
sick.  They are all graded now, so let us know if for some reason you
can't see your graded work.

Assignment 3 was released last night, but we've made a small addition
to the finger exercise on parametric lists.  We've asked you to also
implement @racket[cons] and @racket[empty] @emph{as methods}.  This
will help when you try to abstract things to a super class.  There is
also a hint about using built-in values that have names that clash
with methods.

@section*{Small tweak to assignment 2}

@tt{Wed Jan 19 14:50:15 EST 2011}

There's been a small change to the write-up of
@seclink["assign02"]{assignment 2}.  The @tt{max} of an empty list may
be defined as @racket[-inf.0], negative infinity.


@section*{Ed Fries talk on Tuesday}

@tt{Fri Jan 14 12:13:15 EST 2011}

The Creative Industries program at NEU is hosting Ed Fries, former
Microsoft Vice-President of Game Publishing and co-founder of the
Xbox, who will be speaking on Tuesday, 1/18, from 1:30-3:00pm in the
Raytheon Amphitheater (240 Egan).  Here is Ed's bio from the
@link["http://www.ci.neu.edu/"]{talk announcement}:

@nested[#:style 'inset]{
Ed Fries created his first video games for the Atari 800 in the early
1980s. He joined Microsoft in 1986, and spent the next ten years as
one of the founding developers of both Excel and Word. He left the
Office team to pursue his passion for interactive entertainment and
created Microsoft Game Studios. Over the next eight years he grew the
team from 50 people to over 1200, published more than 100 games
including more than a dozen million+ sellers, co-founded the Xbox
project, and made Microsoft one of the leaders in the interactive
entertainment business. In 2004, Ed retired from his Microsoft Vice
President job to continue his work in the video game business as board
member, advisor and consultant to a broad range of publishers,
independent game developers, and media companies. In 2007 Ed launched
his own startup, FigurePrints, an innovative company that uses 3D
color printing technology to bring video game characters to life. In
the summer of 2010 Ed released "Halo 2600", a "demake" of the Halo
video game series for the Atari 2600.}


@section*{Solutions available}

@tt{Fri Jan 14 11:53:19 EST 2011}

We have decided to make solutions to assignments available after they
are due.  The first assignment's @seclink["soln01"]{solution} is now
on the course webpage.  We welcome design improvements, bug reports,
and feature requests.

@section*{Object-oriented universe}

@tt{Thu Jan 13 22:05:09 EST 2011}

In addition to an object-oriented version of @racket[big-bang], we've
just added an object-oriented version of @racket[universe] for
designing distributed programs (see the documentation for details).
There's no need to use it, @emph{yet}, but it's there if you want to
explore.


@section*{Assignment 2 is out}

@tt{Wed Jan 12 19:14:25 EST 2011}

@seclink["assign02"]{Assignment 2} has been posted and is due at the
usual time on Wednesday.


@section*{Name fix in assignment 1}

@tt{Wed Jan 12 15:21:37 EST 2011}

David Corbett pointed out there was a poor choice of names in the
examples given in @seclink["assign01"]{Assignment 1}.  Originally, we had:

@racketblock[
(define c3+5 (make-cpx 4 5))]

and:

@racketblock[
(define c3+5 (new complex% 4 5))]

We've changed these names to:

@racketblock[
(define c4+5 (make-cpx 4 5))]

and:

@racketblock[
(define c4+5 (new complex% 4 5))]


@section*{Rust talk}

@tt{Tue Jan 11 10:38:56 EST 2011}

Dave Herman's talk on the programming language Rust is today at 3:30 in WVH 366.

Dan's office hours will be changed today to 2:30–3:30 and 5:00–6:00.

@section*{Office hours}

@tt{Tue Jan 11 00:30:48 EST 2011}

@seclink["General"]{Office hours} have been finalized for the course staff.

@section*{The Rust Programming Language}

@tt{Mon Jan 10 19:24:32 EST 2011}

@link["http://www.ccs.neu.edu/home/dherman/"]{Dave Herman}, PRL
alumnus, Mozilla Labs Research Engineer, and JavaScript committee
member, is giving a talk on Mozilla's new programming language, Rust.
If you want to hear about Rust, join us tomorrow, Tuesday, at 3:30pm.
(The location hasn't been nailed down, but it will be posted here as
soon as it is.)

@section*{Initial partnership assignments}

@tt{Mon Jan 10 07:10:12 EST 2011}

@verbatim{
pair 001: mcoppola, mcarroll, naraghi
pair 002: lindeman, robbyk
pair 003: dcorbett, chiner
pair 004: laplante, rpless
pair 005: demaior, kardon
pair 006: mmitlin, forrest
pair 007: morehd, gierb
pair 008: hloople, petey3
pair 009: pfurtado, dmsilva
pair 010: fungi, gressen
pair 011: jgramm, florence
pair 012: hfried7, ngoldman
pair 013: jccoates, markefus
pair 014: tswartz, schwarta
pair 015: wconlon, aldwin
pair 016: ekelly, maxgoren
pair 017: fnimick, wjhsie
}

@section*{Welcome to CS2510H}

@tt{Mon Jan 10 07:10:12 EST 2011} 

We hope you'll have fun.
