#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label class1))

@title[#:tag "assign06"]{2/25: Intelligent Connect 4}

Due: 2/25 (In @bold{2} weeks).

In this assignment, you will write an intelligent computer player for Connect
4, using the techniques we've seen in class. 

You should work on this assignment in several parts, but since they
all go together, there's only one piece to turn in. 

@itemlist[
@item{Improve your original Connect 4 game from last week as you see
fit. Perhaps you incorporate ideas from the official solution, or from
discussions in class, or perhaps you just add more tests for the
corner cases you missed before.}

@item{Improve your game so that you can play repeatedly against the
same opponent, and keep track of how many times each player has won.}

@item{Develop a computer player.  This will be a @tt{World} just like the
human player, but with a faster response time.  Your computer player
doesn't need to do anything intelligent yet.}

@item{Have your computer player evaluate the moves it has available,
and pick an intelligent one based on the current state of the board.}

@item{Have your computer player remember each board position and each
evaluation it makes, and use old ones when they're available.}

@item{Have your computer player change its assessment of moves that
caused it to lose.  Note that a forced move that results in a loss is
a losing move.  }

@item{Introduce randomness into your computer player's behavior.  Run
two computer players against each other.  How often does the player
that goes first win?  How long does it take before your computer
players get good at the game?  Provide empirical data on this in your
solution.}
]