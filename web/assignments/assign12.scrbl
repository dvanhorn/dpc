#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label class1))

@title[#:tag "assign12"]{4/6: Final Project, Parts 1 & 2}

The final project in the class is to implement a distributed,
multi-player version of the Dice Wars game, with an intelligent
computer player.  The first part of the project is due April 6, at
midnight.

To start with, you should try
@link["http://www.gamedesign.jp/flash/dice/dice.html"]{playing the game} to get
a feel for how it works.  The rules are described
@link["http://jayisgames.com/archives/2006/06/dice_wars.php"]{here}, and some
interesting discussion can be found
@link["http://www.metafilter.com/53521/Dice-War-is-Six-Sided-Hell"]{here}.

The basic idea is as follows:  
@itemlist[
@item{The game board consists of a collection of regions, each owned by a
particular player.  Each region is adjacent to some other regions, and consists
of a set of grid locations.}

@item{Each region has some number of six-sided standard dice on it, between 1
and 10.}

@item{Each player takes turns, first attacking other players, and then
recieving new dice.}

@item{During your turn, you can have one territory attack another only if they
are adjacent.  When you attack, all the dice on each side are rolled.  Whoever
rolls the higher number, wins.  If the defender wins, all but one of the
attacker's dice are removed.  If the attacker wins, all of the defender's dice
are removed, and the attacker moves all but one of their dice into the new
region, leaving one die behind.}

@item{Of course, this implies that you can't attack with a region that has just
one die.}

@item{You can attack as many times as you like during your turn.}

@item{After your turn, you get more dice.  The number of new dice is the
maximum number of adjacent regions that you control.  These new dice are
randomly distributed among your regions that don't already have 10 dice.}

@item{Then it's the next player's turn.}

@item{If you ever have no territory, you lose.  If you're the last player, you
win.} 
]

To begin with, you can choose @bold{one} of two tasks to develop the game.  

@itemlist[#:style 'ordered
@item{Implement a Universe server which accepts messages from clients to manage
connections, starts the game when appropriate, manages the game play,
determines who wins each battle and who's turn it is, and notifies all players
about wins and losses. }

@item{Implement a World client that draws the world on the screen, allows the
player to choose what moves to make, renders what happens when the player and
the others play, and generally displays the game state.}
]

Whichever you choose, you will have to determine how to test your system in the
absence of the other half (or implement some portion of the other half). 

In class, we designed both the data definition for representing game states on
the server, and the communication protocol for messages between clients and
servers.  The defintion of the server data is useful, and we suggest that you
use it.  The defintion of the protocol is mandatory---you @emph{must} follow it
so that your programs work with others.

@section{Data Defintion}

@verbatim|{
A Board is (board% [Listof Player] [Listof Region] [Listof Player])
Interp: The first list is the current players, in the order that they take their
turn.
The second list is the regions on the board.  The order must be maintained.
The third list is the players that are only observing.

A Region is (region% [Listof Number] Player Number)
Interp: the first list is the neighbor regions, as indexes into the list of
regions in the Board.  The Number is the quantity of dice.

A Player is (player% Number Name IWorld)
The Number is the player number that the server uses to identify the player.
The Name is an arbitrary String.
The IWorld is the world that this player controls.
}|

To serialize boards, we change the data definition as follows:

@verbatim|{
A SerialBoard is
(List [Listof SerialPlayer]
      [Listof SerialRegion])
There is no need to send the list of observers.

A SerialRegion is (List [Listof Number] SerialPlayer Number)

A SerialPlayer is (List Number String)
IWorlds are not useful off of the server.
}|

@section{Protocol}

@subsection{Player Messages}

@itemlist[
@item{@r['done] indicates that the player is done with their turn.}
@item{@r['(attack Number Number)] indicates that the player wishes to have the
Region indicated by the first number attack the Region indicated by the second
Number.}
@item{@r['(name String) indicates that the player should be known by the
indicated name.]}]

@subsection{Server Messages}

@itemlist[
@item{@r['turn] indicates that the player that receives the message should take
their turn.}
@item{@r['illegal] indicates the the player sent an @r[attack] message that
involved incorrect regions.}
@item{@r['error] indicates the the player sent a message out of turn, or the
message was ill-formed.}
@item{@r['(attack Number Roll Number Roll SerialBoard)] represents an attack
that happened on the board (not necessarily by the player that receives the
message).  The first number is the attacking region, the second number is the
defending region.  The Rolls are the dice rolls of the two regions.  The
SerialBoard is the new board state.  A @r[Roll] is a @r[[Listof Number]], with
each number between 1 and 6.}
@item{@r['(new-state SerialBoard)] indicates a state update for any other
reason, such as new dice allocation.}
@item{@r['(start Number SerialBoard BoardRep)] tells the recieving player that
the game is starting, that they are the player indicated by the given number,
that the board is as described by SerialBoard, and that the board layout may be
drawn as BoardRep.}
]

Here is a data definition for @r[BoardRep] and a simple example of a
two-player board:

@verbatim{
(define WIDTH 7)
(define HEIGHT 5)
;; A BPosn is a (list [0,WIDTH) [0,HEIGHT))

;; A BoardRep is a [Listof (list Number [Listof BPosn])]

;; Interp: a BoardRep is a list of territories, where each 
;; territory is owned by a player (indicated by number) 
;; and consists of a set of continguous board positions.

;; Invariants:
;; - Each list of positions is disjoint from the other lists.
;; - Each list of positions is contiguous.
;; - Each list of positions consists of distinct elements.
;; - All of territories are contiguous.

(define sample-board
  '((1 ((0 2) (0 3) (1 2) (1 3)))
    (2 ((0 0) (0 1)))
    (2 ((1 4) (2 4) (2 3)))
    (1 ((2 0) (3 0) (4 0) (5 0) (5 1)))
    (1 ((1 1) (2 1) (2 2) (3 2)))
    (2 ((3 3) (4 3) (4 2) (5 2) (6 2)))
    (2 ((6 0) (6 1)))
    (1 ((4 4) (5 4) (5 3) (6 4) (6 3)))))
}