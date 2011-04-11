#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label class1))

@title[#:tag "assign13"]{4/20: Final Project, Parts 2 & 3}

Due: 4/20.

This is the final assignment for the class.  

In this assignment, you will do two things:

@itemlist[
@item{Implement the other portion of the @seclink["assign12"]{first project assignment}. You are free to improve your original solution as well.}

@item{Implement an intelligent computer player, using the techniques discussed
in @seclink["lec26"]{class}.}
]

The requirements for this solution are not as specific as for some previous
assignments.  We expect that you implement a working server, client, and
computer player, and that your computer player intelligently evaluates
positions and considers the future.  However, the primary grading criteria is
design; we will @emph{not} grade based on how well your computer player plays,
as long is it makes reasonably intelligent decisions.

There will be a tournament during the final exam period between computer
players.  You are welcome to improve your computer player after the due date
for the tournament.  

