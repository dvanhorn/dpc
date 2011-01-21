#lang scribble/manual
@(require "../utils.rkt"
          (for-label class0))

@title[#:tag "assign02"]{1/19: Zombie}

Due: 1/19 [@seclink["soln02"]{Solution}].

Language: @racketmodname[class0].

@itemlist[#:style 'ordered 
 @item{@bold{Zombie!}
       
        For this exercise, you will design and develop an interactive
        game called @emph{Zombie!}.  In this game, there are a number
        of zombies that are coming to eat your brains.  The object is
        simple: stay alive.  You can maneuver by moving the mouse.
        The player you control always moves toward the mouse position.
        The zombies, on the other hand, always move toward you.  If
        the zombies ever come in contact with you, they eat your
        brains, and you die.  If two zombies happen to come in to
        contact with each other, they will mistakenly eat each other's
        brain, which it turns out is fatal to the zombie species, and
        so they both die.  When a zombie dies, the zombie flesh
        will permanently remain where it is and should any subsequent
        zombie touch the dead flesh, they will try to eat it and therefore die
        on the spot.  Survive longer than all the zombies, and you
        have won the game.

	(This game is based on the @emph{Attack of the Robots!} game
	described in @link["http://landoflisp.com/"]{Land of Lisp}.
	Unlike the Land of Lisp version, this game is graphical and
	interactive rather than text-based.  Hence, our game doesn't
	suck.)

	Here is an animated image capturing a play of the game:
	@image["assignments/zombie-img/i-animated.gif"]{Zombie! play}

	Once you have a working version of the game, add the following
	feature: whenever the user does a mouse-click, the player
	should be instantly teleported to a @emph{random} location on
	the screen.}

  @item{@bold{Finger exercises: Designing classes}

        Design classes to represent lists of numbers.  Implement the
	methods @tt{length}, @tt{append}, @tt{sum}, @tt{prod},
	@tt{contains?}, @tt{reverse}, @tt{map}, and @tt{max}.  Note
	that @tt{max} raises some interesting design decisions in the
	case of the empty list.  One solution is to define the
	@tt{max} of the empty list as negative infinity,
	@racket[-inf.0], a number smaller than every other number
	(except itself).  You may not use @racket[class0] lists to
	implement these classes.}

 ]
