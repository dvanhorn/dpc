#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label class1))

@title[#:tag "assign12"]{Final Project, Parts 1 & 2}

The final project in the class is to implement a distributed, multi-player
version of the Dice Wars game, with an intelligent computer player.

To start with, you should try
@link["http://www.gamedesign.jp/flash/dice/dice.html"]{playing the game} to get
a feel for how it works.
The rules are described
@link["http://jayisgames.com/archives/2006/06/dice_wars.php"]{here}.  

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

To begin with, you can choose one of two tasks to develop the game.  

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

@;{

Design:

Board = (board% [Listof Player] [Listof Region])

Region = (region%
	  [Listof Idx]
	  Player
	  NumDice)

Player = (player% Number Name WorldID) ;; worldid only on server

State serialization
SerialBoard =
([Listof SerialPlayer]
 [Listof SerialRegion])

SerialPlayer = (Number Name)

SerialRegion = ([Listof Number] SerialPlayer Number)

Player Msg:
--------------

'pass
'(attack Idx Idx)

Server Msg:
--------------
'turn
'illegal ;; attack involved illegal squares
'error ;; message had wrong syntax or was out of turn
'(attack Idx Num ;; region, roll
	 Idx Num ;; region, roll
	 State) ;; the new state
'(start State ;; the initial board state
        BoardRep) ;; A representation of the board squares

}