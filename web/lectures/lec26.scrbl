#lang scribble/manual

@(require "../utils.rkt")

@title[#:tag "lec26"]{4/7: AI Dice Player}

@section{Announcements}

The next assignment, which is the rest of the project, will be up
later today.   

@section{AI Dice Player}

Start with a dumb player.

Then design a smart one.

@section{Game Trees}

Initial configuration, then each player takes a turn.

There's a hard-to-summarize series of pictures here.

Strategy:

 - generate all possibilities
 - evaluate each move
 - play game according to your evaluation

How do we evaluate: minimax.

@section{Approximating}

Set a bound on the number of moves.

Numerically evaluate based on board position.

@section{Dealing with chance}

Assume that more dice always beats fewer.

Assign fractional dice for reinforcements.

