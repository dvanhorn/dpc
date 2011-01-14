
#lang scribble/manual
@(require "unnumbered.rkt")

@(require (for-label class0))
@(require (for-label class0/universe))

@title*{Blog}

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
