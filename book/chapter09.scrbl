#lang scribble/manual
@(require "../utils.rkt"
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class1 check-expect define-struct ... length
				numerator denominator))
	  (for-label 2htdp/image)
	  (for-label (only-in test-engine/racket-tests check-expect))
	  (for-label class1/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class2))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@title[#:tag "lec10"]{2/10: Being Smart}


@section[#:tag-prefix "lec10"]{Announcements}


@itemlist[#:style 'ordered

@item{There will be no practice exam.}
@item{PDF Course notes are posted.}
@item{Next assignment due in 2 weeks.}
@item{The exam is in 110, not 108.}
@item{Study for the exam.}
@item{Follow directions; including this one.}
]

@section{The 3-In-A-Row Game}

Today we're going to look at this game we made up.  It's called 3-in-a-row.
The object of the game is to place three peices in a row.  You take
turns playing against the computer.

Here is an example of playing the game. You start with an empty board.
You can place a peice by pressing a number, which puts the peice at
the slot with that number (counting from zero).

...

What we're going to work on today is coming up with a smart computer
player.  In class we'll develop one for the 3-in-a-row game.  On the
current homework, you

@codeblock{
;; A Board is a [Listof Color]
;; A Color is one of 
;; 'red 
;; 'black
}

What's wrong with this?  It's hard to represent the current board
state that is all empty.

We modify our data definition as:

@codeblock{
;; A Board is a [Listof Color]

;; A Color is one of:      Interp:
;; 'red                    - the player
;; 'black                  - the computer
;; 'white                  - blank
}

What's an important function for us to write to manipulate a board
like this?  How about determinining who, if anybody, has won the game?

Let's write the function:

@codeblock{
;; player-win? : Board -> Boolean
;; Are there three red peices in a row on the board?
}

Idea: use an accumulator to remember how many red peices in a row
we've seen so far.  When you see a non-red peice, you reset the
accumulator.  When you see a red peice you increment the accumulator.
When the accumulator gets to three, the player has won.

So we define a local function that adds an accumulator argument.

@codeblock{
(define (player-win? b)
  ;; win-helper : Board Number -> Boolean
  ;; Acc : hom many red peices we've seen so far
  (local [(define (win-helper b acc)
            (cond [(= acc WIN) true]
		  [(empty? b) false]
		  [else
		   (cond [(symbol=? 'red (first b))
			  (win-helper (rest b) (add1 acc))]
			 [else
			  (win-helper (rest b) 0)])]))]
     (win-helper b 0)))
}

Now let's program the computer-win? predicate:

@codeblock{
(define (player-win? b)
  ;; win-helper : Board Number -> Boolean
  ;; Acc : hom many red peices we've seen so far
  (local [(define (win-helper b acc)
            (cond [(= acc WIN) true]
		  [(empty? b) false]
		  [else
		   (cond [(symbol=? 'red (first b))
			  (win-helper (rest b) (add1 acc))]
			 [else
			  (win-helper (rest b) 0)])]))]
     (win-helper b 0)))
}

Apply the abstraction recipe and we see to abstract out the symbol and
make it a parameter to the function:

@codeblock{
(define (win? p b)
  ;; win-helper : Board Number -> Boolean
  ;; Acc : hom many red peices we've seen so far
  (local [(define (win-helper b acc)
            (cond [(= acc WIN) true]
		  [(empty? b) false]
		  [else
		   (cond [(symbol=? p (first b))
			  (win-helper (rest b) (add1 acc))]
			 [else
			  (win-helper (rest b) 0)])]))]
     (win-helper b 0)))

(define (win-player? b)
  (win? 'red b))

(define (win-computer? b)
  (win? 'black b))
}

Now...

@codeblock{
;; play : Color Number Board -> Board
;; place the appropriate piece on the board
}

Is the board going to be a constant size?

Why are you asking?

Because if the board is a constant size, then the board could be
represented with a vector.

Looking at our win? predicate suggests the list representation is
good.

But our play function suggests that an @emph{indexed} view of our data
is a appropriate.

So you can think of a list with two views: one is recursive, the other
is indexed; each position in the list is numbered and we can refer
to values in the list according to the index at which they reside.

Why do we need play, when you just call play with the same arguments?

Because what if we changed the representation of the board.
Also, play has it's own purpose.

@codeblock{
(define (play c n b)
  (list-set b n c))
}

Game works like this: we play, computer plays, we play, computer
plays, etc. until someone wins.


A World is going to have two things: a board and a representation of
the computer player.  We'll see the computer data definition later.

@codeblock{
(define-class world%
  (fields board computer)
  ;; -> Scene
  ;; Render this world as a scene.
  (define/public (to-draw)
    (overlay 
     (foldr beside empty-image (map (lambda (clr) (circle 20 "solid" clr))
				    (field board)))
     (empty-scene 500 500)))

  ;; KeyEvents
  ;; Handle number keys by playing and N-key by reseting the game.
  (define/public (on-key k)
    (cond [(number? ...) 
	   (world% (play 'red (string->number k) (field board))
		   (field computer))]
	  ...)))

(define INITIAL (build-list SIZE (lambda (_) 'white)))

(big-bang (world% INITIAL 'computer)) 
; Bogus value for the computer, since we don't use it currently
}

Now we can play, but the computer never plays.  (The white circles
don't look great).

Now let's think about how the computer is going to play.

What should a computer player do?  What should it's interface be?
What's the job of a computer player?

Looks at the board and picks a move.

Reducing this to a decision problem.  Where do we want to play (OK,
not a decision problem).

[Missed the discussion at this point.]

We've got a new board that represents where the player has played.

How can get the computer to choose a place to play?

Make a new board, play 'black wherever the computer picks to
play (giving the board that we just played on.

What would be the problem with the computer returning a board?

We want to have a strict seperation of concerns.

Chooses a number and we play there.

@codeblock{
(define-class computer%
  (define/public (pick brd)
    ...))
}

Q: Speaking of cheating, what do we do to enforce that the player make
a legal move?

Let's implement the simplest strategy for the computer.  What is the
simplest strategy for the computer playing?  Play on Zero.

Now play until player wins.  What happened?  Nothing.  We never used
the win? predicate even though we wrote it.

Let's do on-key.

What are the possible outcomes?

- Player wins
- Computer wins
- It's a draw

When the game is over, should we respond to key board events?  No.
That world is going to behave pretty differently than this world.  The
best way to represent that is with another class.

@codeblock{
;; A DoneWorld
}

What information does a done world need to maintain?

- Who won
- The computer (since the computer is going to learn as it plays)

It wouldn't learn much if we reset what it had learned after each
round.

We draw by saying who won.  We're going to handle the N-key event by
start a new game, which will start with the computer we have.

When we have a tie, go to done, saying there was a tie.

Nice design pattern for implementing world games; for example, in tron
or connect four: the universe has multiple states it could be in (no
worlds connected, one connected, two connected).  Also comes up in
local games: states of the world (playing, done).

State machine, transition system.  Pattern comes up all the time.

@verbatim{
---> PLAY ---------> DONE
       ^              |
       +--------------+
}

Models nicely in an OO language.

We no longer need to conditionally something if we're in the done
state or play state -- we can just rely on the object itself being a
done object or a play object.  The conditional goes away.

How can we check for a tie?

There are no white pieces.

@codeblock{
(define (tie? b)
  (not (member 'white b)))
}

Still failing to do a bunch of things:

- Checking that player doesn't cheat.
- The computer plays in a dumb (and illegal) way

Let's consider how to make the computer smarter.

Start with seven peices.

How can pick a good spot?  How can pick the best spot?

- Pick the first spot that you win?

What if the board is blank?

So *if* you can win, you want to win.

- Block the other player from winning.

Which do we want to do first?

If you can win, win.  If you can't but you can block a win, block.
Otherwise, ... who knows.

We don't want to think in advance.

Might want to consult our past experience (our predictions).

We got to this situation and played at the end and always lost.

We haven't decided how much to remember from previous plays.

Indexed data and lists (works just fine for small lists).

Play -- you lose your turn if you play off the board, or if you pick
a spot that has already been played.

First goal: play in an empty space.

How can find an empty space?  Pick a number, see if it's empty.

Idea: iterate through numbers and pick the first one that is empty.

Is there a problem here?  We might go off the end.

Invariant: never asked to play on a full board.  So we don't have to
worry about that case.

Let's make sure we are maintaining that invariant.  We make sure
there's an empty spot just before asking the player to pick.

We have the computer play, then not checking to see if the computer
won -- we just keep on playing.

Let's fix that.


EVALUATION

How do we evaluate a position?

W C P P W

What information do we get out?

Summarize by where we should play or false.

Before we do that, we should gather information on all possible plays.


We have 5 spots and we need to evaluate each position.

@verbatim{
_ _ _ _ _
}

What are the possibilities:

- it's full
- don't know
- we win if we play here
- we lose if we play here

Sufficient to capture everything we could know.

Other more compicated games might need more information.  Which comes
from refining the unkown bit to reason about "likely" outcomes.

Develop a function that takes a board and evaluates it.

Develop a data definition for an evaluation:

@codeblock{
;; An Eval is a [Listof Result].
;; A Result is one of:
;; - 'win
;; - 'lose
;; - 'full
;; - 'unkown

;; Board -> Eval
(define/public (evaluate brd)
  ...)
}

Maybe have an evaluate-element function.  It needs the 
the index and the board.

@codeblock{
(local [;; Board Number -> Result
	(define (eval-elem index) ...)]
  (build-list SIZE evaluate-elem))
}

How do we evaluate a position?

@codeblock{
(local [;; Board Number -> Result
	(define (eval-elem index) 
	  (cond [(not (symbol=? 'white (list-ref brd index))) 'full]
		[(computer-win? (play 'black index brd)) 'win]
		[(player-win? (play 'red index brd)) 'lose]
		[else 'unkown]))]
  (build-list SIZE evaluate-elem))
}	

Going back to pick:

if there's a win, play there; otherwise if there's a block, play there;
otherwise play at the first place we can.

We need a function that will tells us the first postion

@codeblock{
;; find-res : Eval Result -> (U Number False)
;; find the first occurrence of the given result.
}

But what if the whole board is bad for us?  If we say, where can I
when, what should we get back.

What if we return -1 instead?  Why is false a better choice?  It is a
different KIND of value that we won't confuse with other kinds of 
indexes.

Write obvious recursive function.  What's the problem? Might try to
add one to false.  Avoid doing that.

Now, let's go back to pick.

Now play.  Tada: we block the player after it plays two.

But we could have done better.  When did the program go wrong?
With it's first move.

It makes the mistake again and again.


Needs to remember that playing in spot 0 is not going to work out
well.

What does our computer need to do in order to learn things?


A list of boards and where it went.

What tells us where to move?  Evaluations.

Save previous evaluations.

Tie evaluations to end result of that game.

Idea for next time: mark evaluations as a loss when you lose.


@section{Indexed Views on Data}

@section{State Transition Patterns}

@section{Designing a Computer Player}

@subsection{A simple player}
@subsection{Evaluating positions}
@subsection{Picking a move}
@subsection{Remembering the past}

@section{Completed code}

As written in class.

@verbatim|{
#lang class2
(require 2htdp/image class2/universe)

;; A Board is a [Listof Color]
;; A Color is one of
;; - 'white - blank
;; - 'black - the computer
;; - 'red - the player

(define WIN 3)
(define SIZE 10)

;; player-win? : Board -> Boolean
(define (player-win? b)
  (win-helper 'red b 0))

;; computer-win? : Board -> Boolean
(define (computer-win? b)
  (win-helper 'black b 0))

;; tie: Board -> Boolean
(define (tie? b)
  (not (member 'white b)))

;; win-helper : Color Board Number ->  Boolean
;; Acc : how many pieces we've seen so far  
(define (win-helper sym b acc)
  (cond [(= acc WIN) true]
        [(empty? b) false]
        [else (cond [(symbol=? sym (first b))
                     (win-helper sym (rest b) (add1 acc))]
                    [else (win-helper sym (rest b) 0)])]))

;; play : Color Number Board -> Board
;; place the appropriate piece on the board
(define (play c n b)
  (cond 
    [(> n (add1 SIZE)) b]
    [(symbol=? 'white (list-ref b n))
     (list-set b n c)]
    [else b]))


;; list-set : Listof[X] Nat X -> Listof[X]
;; replaces the kth element of l with e
(define (list-set l k e)
  (cond [(= k 0) (cons e (rest l))]
        [else (cons (first l) (list-set (rest l) (sub1 k) e))]))

;; find-res : Evaluation Result -> Number or False
;; find the first occurence of this result
(define (find-res eval r)
  (cond [(empty? eval) false]
        [else (cond [(symbol=? r (first eval)) 0]
                    [else 
                     (local [(define result (find-res (rest eval) r))]
                       (cond [(number? result) (add1 result)]
                             [else false]))])]))

;; A Computer implements:
;; pick : Board -> Number
(define-class computer%
  
  ;; An Evaluation is a [Listof Result]
  ;; A Result is one of 'Win 'Block 'Full 'Unk
  
  ;; evaluate Board -> Evaluation
  (define/public (evaluate brd)
    (local [(define (evaluate-elem index)
              (cond [(not (symbol=? 'white (list-ref brd index))) 'Full]
                    [(computer-win? (play 'black index brd)) 'Win]
                    [(player-win? (play 'red index brd)) 'Block]
                    [else 'Unk]))]
      (build-list SIZE evaluate-elem)))
  
  ;; Pick a space to play in
  ;; Invariant : brd is not full
  ;; Board -> Number
  (define/public (pick brd)
    (local [(define eval (evaluate brd))]
      (cond [(member 'Win eval)
             (find-res eval 'Win)]
            [(member 'Block eval)
             (find-res eval 'Block)]
            [else
             (find-res eval 'Unk)]))))

(define SCENE (empty-scene 500 500))

;; A World is (world% Board Computer)
(define-class world%
  (fields board computer)
  (define/public (to-draw)
    (overlay
     (foldr beside empty-image (map (λ (clr) (circle 20 "solid" clr)) (field board)))
     SCENE))
  
  (define/public (on-key k)
    (cond [(number? (string->number k)) 
           (local [(define new-board (play 'red (string->number k) (field board)))]
             (cond [(player-win? new-board) (done% "Player Win!" (field computer))]
                   [(tie? new-board) (done% "Tie!" (field computer))]
                   [else
                    (local [(define second-board (play 'black 
                                                       (send (field computer) pick new-board)
                                                       new-board))]
                      (cond [(computer-win? second-board) (done% "Computer Win!" 
                                                                 (field computer))]
                            [(tie? second-board) (done% "Tie!" (field computer))]
                            [else (world% second-board (field computer))]))]))]
          [(key=? "n" k) (world% INITIAL (field computer))]
          [else this]))
  )

;; A DoneWorld is (done% String Computer)
(define-class done%
  (fields msg computer)
  
  (define/public (to-draw)
    (overlay (text (field msg) 30 "black")
             SCENE))
  
  (define/public (on-key k)
    (cond [(key=? "n" k) (world% INITIAL (field computer))]
          [else this])))

(define INITIAL (build-list SIZE (λ (i) 'white)))

(big-bang (new world% INITIAL (computer%)))
}|