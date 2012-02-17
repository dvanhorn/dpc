#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/0 define-struct ... length))
	  (for-label 2htdp/image)
	  (for-label class/universe))

@title{Universe}

@section{A look at the Universe API}

In this chapter, we're going to start looking at the design of
multiple, concurrently running programs that communicate we each
other.  We will use the @racket[universe] system as our library for
communicating programs.

The basic universe concept is that there is a ``universe'' program that
is the adminstrator of a set of world programs.  The universe and the
world programs can communicate with each other by sending messages,
which are represented as S-Expressions.

So far we have focused on the design of single programs; we are now
going to start looking at the design of communicating systems of
programs.

In addition to these notes, be sure to read the documentation on
@secref[#:doc '(lib "teachpack/teachpack.scrbl") #:tag-prefixes
'("2htdp") "world2"].

@section{Messages}

A message is represented as an S-Expression.  Here is there is their
data definition:

An S-expression is roughly a nested list of basic data; to be precise
an S-expression is one of:

@itemlist[
 @item{a string,}
 @item{a symbol,}
 @item{a number,}
 @item{a boolean,}
 @item{a char, or}
 @item{a list of S-expressions.}]

The way that a world program sends a message to the universe is by
constructing a package:

@#reader scribble/comment-reader
(racketblock
  ;; A Package is a (make-package World SExp).
)

The world component is the new world just like the event handler's
produced for single world programs.  The s-expression component is a
message that is sent to the universe.

@section[#:tag "counter"]{Simple world}

As a simple example, let's look at a world program that counts up and
sends messages to a universe server as it counts.  In this simple
example there is only one world that communicates with the server, and
the server does nothing but receive the count message (it sends no
messages back to the world).

Let's start with the counting world program, which does not
communicate with any server, it just counts:

@#reader scribble/comment-reader
(racketmod
  class/0
  (require class/universe)
  (require 2htdp/image)

  ;; Scene is 300x100 pixels
  (define WIDTH 300)
  (define HEIGHT 100)

  ;; A CounterWorld is a (new cw% Natural)
  ;; and implements
  ;; - tick-rate : -> Number
  ;;   Tick rate for counting.
  ;; - on-tick : -> CounterWorld
  ;;   Increment counter world state.
  ;; - to-draw : -> Scene
  ;;   View counter world state as a scene.
  (define-class cw%
    (fields n)
    (define (tick-rate) 1)
    (define (on-tick)
      (new cw% (add1 (send this n))))   
    (define (to-draw)
      (overlay (text (number->string (send this n)) 40 "red")
               (empty-scene WIDTH HEIGHT))))
  
  ;; Run, program, run!
  (big-bang (new cw% 0))
)

When you run this program, you see the world counting up from zero.

@section{Simple world, broadcasting to server}

Now let's modify our program so that it does some simple communication
with a server.  As an initial design, we'll make a program that simply
notifies a server as it counts.  In other words, our program will only 
engage in one-way communication by broadcasting data to the server.

So what exactly do we want to broadcast?  If we want to communicate
the current state of the world, we may be tempted to try to
communicate the current @racket[CounterWorld] value; but remember that
the current world state is an @emph{object}, which isn't included in
the data definition of messages.  Moreover, the current state includes
not only data, but functionality: functionality for handling tick
events, viewing the current count as an image, etc.---all of which are
things the server doesn't really need.  To communicate the essence of
what state the @racket[CounterWorld] is in, all we really need to send
is the current count: a number, which fortunately does fall under the
set of message values.

Thinking about the message protocol for this simple scenario, the
client and server communications will look like this:

@verbatim{
          Client                        Server
--------------------------             ---------
Event     State    Message                        

Bang   (new cw% 0)         =========>  
Tick   (new cw% 1)   1     --------->
Tick   (new cw% 2)   2     --------->
Tick   (new cw% 3)   3     --------->
...         ...     ...       ...
}

Here, the @tt{==>} arrow indicates the world registering with the
server.  This is the one time communication that establishes a
dialogue (really, in this case, a monologue) between the client and
server.  After registering with the server, the client will send its
current count, indicated wtih @tt{-->} arrows, as it ticks along.

In this table we show the state of the client and the events that
occur.  With each event, which potentially changes the state, we show
what message is sent to the server.  At this point the server is
opaque---we don't know or really care what state the server is in and
we assume that the server doesn't send any message back to the client.

Now to register this program with a universe server, we need to
implement a @racket[register] method that produces a string that is
the IP address of the server.  (Since we're going to run the universe
and world on the same computer, we will use @racket[LOCALHOST] which
is bound to the address of our computer.)

@filebox[@r[cw%]]{
@#reader scribble/comment-reader
(racketblock
  ;; register : -> String
  ;; IP address of server
  (define (register) LOCALHOST)
)
}

Now when you run this program you will see the world program try to
connect to the universe, but since we have not written---much less
run---the server, it cannot find the universe.  After a few tries, it
gives up and continues running without communicating with the
universe.

@section{Simple universe, receiving broadcasts}

Now let's write a simple server that receives the message from the
counter world client.  We could write the server in the same file as
the client, but since these are really two separate programs that talk
to each other, let's emphasize that by writing the server in its own
tab.

A universe program is similar to a world program: it's a program that
responds to events.  The difference is in the kind of events that can
occur. The most important events are already shown in our protocol
diagram: 
@itemlist[
@item{a new world starts communicating with the server,}
@item{a world sends a message.}]

When these events occur, the server reacts by calling the appropriate
method, in this case @racket[on-new] and @racket[on-msg].

We'll be working with the OO-style universe, but you should read the
documentation for @racketmodname[2htdp/universe] and translate over
the concepts to our setting as you've done for @racket[big-bang].

As it turns out, if you leave these methods off, the universe library
will do something sensible, namely nothing.  So for our simple counter
program, the following works:

@codeblock{
#lang class/0
(require class/universe)
(define-class cu%)

;; Run, server, run!
(universe (new cu%))
}

some text here

@#reader scribble/comment-reader
(racketmod
  class/0
  (require class/universe)

  (define-class universe%
    ;; IWorld -> Bundle
    (define (on-new iw) ...)
  
    ;; IWorld S-Expr -> Bundle
    (define (on-msg iw m) ...))
)

When a world registers, the @racket[on-new] method is called with an
IWorld value.  An IWorld value opaquely represents a world, that is
you do not have the ability to examine the contents of the value, but
you can compare it for equality with other IWorld values using
@racket[iworld=?].

When a world sends message, the @racket[on-msg] method is called with
the IWorld representing the world that sent the message and the S-Exp
message that was sent.

In both cases, the method must produce a bundle:

@#reader scribble/comment-reader
(racketblock
  ;; A Bundle is a (make-bundle Universe [Listof Mail] [Listof IWorld]).
)

The universe component is the new state of the universe; the list of
mail is a list of messages that will be sent back to the worlds (more
on this in a moment), and the list of worlds are worlds that the
server has chosen to disconnect from.


For the purposes of our example, the universe maintains no state (the
class has no data).  When a new world registers, we do nothing, and
when a world sends a message, we also do nothing, send nothing in
response, and disconnect no worlds:

@#reader scribble/comment-reader
(racketmod 
  class/0
  (require class/universe)

  (define-class universe%
    ;; IWorld -> Bundle
    (define (on-new iw) 
      (make-bundle this empty empty))
    
    ;; IWorld S-Expr -> Bundle
    (define (on-msg iw m)
      (make-bundle this empty empty)))
  
  (universe (new universe%)) 
) 

Running this program launches the universe server, making it ready to
receive registrations from worlds.  After starting the universe
server, if we switch back to the world program tab and run it, we'll
see that it successfully registers with the universe and the universe
console reports that the world signed up with it.

Thrilling.

At this point, the world registers, but never sends any message to the
server.  Now let's modify the world program to notify the server when
it ticks by sending it's current count as a message.

That requires changing our @racket[on-tick] method from:

@filebox[@r[cw%]]{
@#reader scribble/comment-reader
(racketblock
  (define (on-tick)
    (new cw% (add1 (send this n))))
)
}

to one that constructs a package:

@filebox[@r[world%]]{
@#reader scribble/comment-reader
(racketblock
  (define (on-tick)
    (make-package (new cw% (add1 (send this n)))
		  (add1 (send this n))))
)
}

Now let's re-run the world program.  Notice that messages are being
received by the server in the console.

@section{Migrating computation from client to server}

Now let's change the system so that "work" of adding is done on the
server side.  When the world ticks it sends its current count to the
server; when the server receives the count, it responds by sending a
message back to the world that is the count plus one.  On the world
side, that means we must now add the ability to receive messages by
implementing the @racket[on-receive] method, which will update the
world state appropriately and the @racket[on-tick] method will do no
computation, but only communication:

@filebox[@r[world%]]{
@#reader scribble/comment-reader
(racketblock
  (define (on-tick)
    (make-package this (send this n)))
  
  (define (on-receive m)
    (new counter-world% m))
)
}

On the server side, we now need to send a message back to the world,
so we need to consider the data definition for mail:

@#reader scribble/comment-reader
(racketblock
  ;; A Mail is a (make-mail IWorld S-Exp).
)

This constructs a message that will be sent to the world represented
by the IWorld value consisting of the S-Exp value.

@filebox[@r[universe%]]{
@#reader scribble/comment-reader
(racketblock
  (define (on-msg iw m)
    (make-bundle this 
		 (list (make-mail iw (add1 m))) 
		 empty))
)
}

If we restart the universe and world, you'll notice that the universe
console is now showing messages going in both directions.

Now let's see an example of multiple world programs communicating with
a single server.

If we were to just write this:

@#reader scribble/comment-reader
(racketblock
  (big-bang (new counter-world% 0))
  (big-bang (new counter-world% 50))
)

the program would wait for the first big-bang expression to finish
evaluating before moving on the second one.  To make it possible to
run many worlds at the same time, the universe library provides the
@racket[launch-many-worlds] form that will evaluate all of its
subexpressions in parallel:

@#reader scribble/comment-reader
(racketblock
  (launch-many-worlds
    (big-bang (new counter-world% 0))
    (big-bang (new counter-world% 50)))
)

Notice that both worlds count independently.

@section{Guess my number}

Now let's a more interesting game.  We'll start by considering the
guess my number game.

In this game, the server is thinking of a number and you have to guess
it.

Here's the server:

@#reader scribble/comment-reader
(racketmod
  class/0
  (require class/universe)

  (define-class universe%
    (fields the-number)
    
    (define (on-new iw) 
      (make-bundle this empty empty))
    
    (define (on-msg iw m)
      (make-bundle this 
		   (list (make-mail iw (respond m (send this the-number))))
		   empty)))

  ;; Number Number -> String
  (define (respond guess number)
    (cond [(< guess number) "too small"]
	  [(> guess number) "too big"]
	  [else "just right"]))

  ;; the universe is thinking of 2.
  (universe (new universe% 2))
)


The universe now has a single peice of data, which is the number it is
thinking of.  It responds to guesses by sending back a string
indicating whether the guess is just right, too big, or too small.

Here is the client:

@#reader scribble/comment-reader
(racketmod
  class/0
  (require class/universe)
  (require 2htdp/image)

  (define-class guess-world%
    (fields status)
    
    (define (on-receive m)
      (new guess-world% m))
      
    (define (to-draw)
      (overlay (text (send this status)
		     40
		     "red")
	       (empty-scene 300 100)))
    
    (define (on-key k)
      (local [(define n (string->number k))]
        (if (number? n)
	    (make-package this n)
	    this)))                    
  
    (define (register) LOCALHOST))

  (big-bang (new guess-world% "guess a number"))
)

The client has a single peice of data, which represents the status of
its guess.  It responds to numeric key events by sending the guess to
the server and ignores all other key events.

The @racket[string->number] function is being used to test for numeric
key events---it works by producing @racket[false] when given a string
that cannot be converted to a number, otherwise it converts the string
to a number.

@section{Two player guess my number}

Now let's write a 2-player version of the game where one player thinks
of a number and the other player guesses.

Here is the server:

@#reader scribble/comment-reader
(racketmod
  class/0
  (require class/universe)

  ;; A Universe is a (new universe% [U #f Number] [U #f IWorld] [U #f IWorld]).
  (define-class universe%
    (fields number
	    picker
	    guesser)
    
    ;; is the given world the picker?
    (define (picker? iw)
      (and (iworld? (send this picker))
	   (iworld=? iw (send this picker))))
    
    ;; is the given world the guesser?
    (define (guesser? iw)
      (and (iworld? (send this guesser))
	   (iworld=? iw (send this guesser))))
    
    (define (on-new iw)
      (cond [(false? (send this picker))
	     (make-bundle
	      (new universe% false iw false)
	      (list (make-mail iw "pick a number"))
	      empty)]          
	    [(false? (send this guesser))
	     (make-bundle
	      (new universe% (send this number) (send this picker) iw)
	      empty
	      empty)]          
	    [else
	     (make-bundle this empty (list iw))]))
    
    (define (on-msg iw m)
      (cond [(and (picker? iw)
		  (false? (send this number)))           
	     (make-bundle
	      (new universe% m (send this picker) (send this guesser))
	      empty
	      empty)]
	    [(picker? iw) ;; already picked a number
	     (make-bundle this empty empty)]
	    [(and (guesser? iw)
		  (number? (send this number)))
	     (make-bundle this 
			  (list (make-mail iw (respond m (send this number))))
			  empty)]
	    [(guesser? iw)
	     (make-bundle this
			  (list (make-mail iw "no number"))
			  empty)])))
  
  ;; Number Number -> String
  (define (respond guess number)
    (cond [(< guess number) "too small"]
	  [(> guess number) "too big"]
	  [else "just right"]))

  (universe (new universe% false false false))
)

The client stays the same!  You can launch the two players with:

@#reader scribble/comment-reader
(racketblock
  (launch-many-worlds
   (big-bang (new guess-world% "guess a number"))
   (big-bang (new guess-world% "guess a number"))))

@include-section{06/exercises.scrbl}