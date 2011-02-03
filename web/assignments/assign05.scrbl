#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label class1))

@title[#:tag "assign05"]{2/9: Networked Connect 4}

Due: 2/9.

In this assignment, you will design and develop an implementation of
Connect 4, a popular "board" game.  As with
@seclink["assign04"]{Assignment 4},  your game will be multi-player
and networked, and you will design both server and client programs.  

The rules of Connect 4 are simple (A more detailed explanation is
available from
@link["http://en.wikipedia.org/wiki/Connect_Four"]{Wikipedia}).   For
the purposes of this assignment, we will begin with a 4 by 4 grid of
open spaces, usually rendered as circles.  The two players take turns
adding pieces to the board.  To play, a player chooses a column, and
places their piece in the lowest open space in the column.  If there
are no available columns to play in, the game is a draw.  

The goal of the game is to get 3 of your pieces adjacent.  The pieces can
be either all in the same row, all in the same column, or in a
diagonal (either falling or rising).  The first player to accomplish
this wins.  

@itemlist[#:style 'ordered 
@item{Implement the game on a 4 by 4 board, with 3 in a row to win.}
@item{Generalize your implementation to support an N by M board, with
O in a row to win. In particular, you must be able to support
@emph{both} the simple version from Problem 1, and the full board: 7
by 6 with 4 to win. Note: this must be in a separate file from your
solution to Problem 1.}
]

@bold{Design Notes}

@itemlist[#:style 'ordered
@item{@bold{Representation:} There are many possible representations
for the grid of board locations, but one of them is dramatically
easier to work with than the others.  Think about all the
possibilities before you choose one.}
@item{@bold{Winning:} The hardest part of implementing Connect 4 is
determining if a player has won.  To solve this problem, it is helpful
to think about transforming the board to make the problem simpler.  In
general, depending on your representation, one winning condition will
be easier to check for than the others.  Try to transform the tricky
cases into the simple cases.}
@item{@bold{Turns:} Note that preventing players from playing out of
turn, or cheating in general, is the responsibility of the universe
server.} 
]