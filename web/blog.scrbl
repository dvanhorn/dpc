
#lang scribble/manual
@(require "unnumbered.rkt")

@(require (for-label class0))
@(require (for-label class0/universe))

@title*{Blog}

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
