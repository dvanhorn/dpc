#lang scribble/manual
@(require "../utils.rkt"
          (for-label class1))

@title[#:tag "assign04"]{2/2: Tron}

Due: 2/2.


 For this assignment, you will design and develop the Tron Lightcycle game.
 The basic idea of this game, if you haven't seen the movie, is that
 two players move around the screen, leaving a trail of where each of the
 players has been.  If a player runs into the trail that they or the
 other player has left behind, or into a wall, they lose.  If the two
 players simultaneously hit each other, or both hit a wall or trail at
 the same time, the game is a tie.

 Here's a screenshot of playing the game:

 @image["assignments/tron1.png"]

 Here's a
 @link["http://www.classicgamesarcade.com/game/21670/Tron-Game.html"]{flash
 game} where you can play the game yourself online.  

@itemlist[#:style 'ordered 
 @item{@bold{Distributed Tron}
 
 The first version of Tron you will develop is a distributed one,
 using the @racketmodname[class1/universe] library.  You will need to
 design two separate parts of the program:

 The server: this will accept connections from two clients,
 communicate with the clients via messages indicating the directions
 the clients want to move, and then send back updated information
 about the positions of both players and the trails on the board.  

 The client: you should only need to @emph{implement} one client, but you
 will @emph{run} two of them, one for each player.  The client will
 draw the world to the screen, recieve messages from the server and
 update the world state in response, allow the user to input their
 desired direction (probably via the arrow keys) and communicate this
 direction to the server.  

 Once you've implemented both the server and client, you'll be able to
 play against your friends and classmates over the network.  

 }

 @item{@bold{Computer Tron}

 Language: @racketmodname[class1]. 

 In this part of the assignment, you'll implement a new kind of
 client---a computer player.  This player will, like the regular
 client, display the world as well as send and recieve messages to and
 from the server.  However, it won't take input from the user; instead
 it will make decisions itself based on the state of the board.  

 There is no requirement for any particular behavior for your computer
 player---you can have it behave randomly, behave dumbly, or be the
 world's best tron player.  We won't grade your assignment based on
 its playing choices, but we encourage you to go wild with your
 choices of how the computer player behaves.  

 }	   


]

