UNIVERSE
========

Announcements:

* New partners have been assigned, see the blog for pair assignments.

* Solutions for assignment 3 are posted.

* Assignment 4 is out.

* Reminder: Van Horn won't be available for his office hours, but
  email the course staff if you need help with something.

* You will need the latest version of the course .plt file for
  universe to work.


A look at the Universe API
--------------------------

The basic universe concept is that there is a "universe" program that
is the adminstrator of a set of world programs.  The universe and the
world programs can communicate with each other by sending messages,
which are represented as S-Expressions.

So far we have focused on the design of single programs; we are now
going to start looking at the design of communicating systems of
programs.

See the documentation for "The World is not Enough".

Key Concepts:

Server (universe) and Clients (worlds)
--------------------------------------

Setting  up a world to communicate with a universe server.


Messages
--------

A message is represented as an S-Expression.  Here is there is their
data definition:

An S-expression is roughly a nested list of basic data; to be precise,
an S-expression is one of:

    a string,
    a symbol,
    a number,
    a boolean,
    a char, or
    a list of S-expressions.

Sending messages (Client-side, world)
----------------

The way that a world program sends a message to the universe is by
constructing a package:

   A Package is a (make-package World SExp).

The world component is the new world just like the event handler's
produced for single world programs.  The s-expression component is a
message that is sent to the universe.

As a simple example, let's look at a world program that counts up and
sends messages to a universe server as it counts.  In this simple
example there is only one world that communicates with the server, and
the server does nothing but receive the count message (it sends no
messages back to the world).

Let's start with the counting world program:

  #lang class1
  (require class1/universe)
  (require 2htdp/image)
  
  (define-class counter-world%
    (fields n)
  
    (define/public (on-tick)
      (new counter-world% (add1 (field n))))
  
    (define/public (tick-rate)
      1)
  
    (define/public (to-draw)
      (overlay (text (number->string (field n))
                     40
                     "red")
               (empty-scene 300 100))))
  
  (big-bang (new counter-world% 0))

[Run this, show counting.]

Now to register this program with a universe server, we need to
implement a @racket[register] method that produces a string that is
the IP address of the server.  (Since we're going to run the universe
and world on the same computer, we will use @racket[LOCALHOST] which
is bound to the address of our computer.)

  (define/public (register) LOCALHOST)

[Run this program, show that it is looking for the universe and can't
find it.]

Now we need to start a universe server that the world can communicate
with.

[Open new tab.]

A universe program, much like a world program, consists of a current
state of the universe and is event-driven, invoking methods to produce
new universe states.

We'll be working with the OO-style universe, but you should read the
documentation for 2htdp/universe and map over the concepts as you've
done for big-bang.

At a minimum, the universe must handle the events of (1) a world
registering with this universe and (2) a registered world sending a
message to the universe.  The first is handled by the @racket[on-new]
method and the second by the @racket[on-msg]:

#lang class1
(require class1/universe)

(define-class universe%
  ;; IWorld -> Bundle
  (define/public (on-new iw) ...)
  
  ;; IWorld S-Expr -> Bundle
  (define/public (on-msg iw m) ...))

When a world registers, the @racket[on-new] method is called with an
IWorld value.  An IWorld value opaquely represents a world, that is
you do not have the ability to examine the contents of the value, but
you can compare it for equality with other IWorld values using
@racket[iworld=?].

When a world sends message, the @racket[on-msg] method is called with
the IWorld representing the world that sent the message and the S-Exp
message that was sent.

In both cases, the method must produce a bundle:

  A Bundle is a (make-bundle Universe [Listof Mail] [Listof IWorld]).

The universe component is the new state of the universe; the list of
mail is a list of messages that will be sent back to the worlds (more
on this in a moment), and the list of worlds are worlds that the
server has chosen to disconnect from.


For the purposes of our example, the universe maintains no state (the
class has no data).  When a new world registers, we do nothing, and
when a world sends a message, we also do nothing, send nothing in
response, and disconnect no worlds:

#lang class1
(require class1/universe)

(define-class universe%
  ;; IWorld -> Bundle
  (define/public (on-new iw) 
    (make-bundle this empty empty))
  
  ;; IWorld S-Expr -> Bundle
  (define/public (on-msg iw m)
    (make-bundle this empty empty)))

(universe (new universe%))


[Run this program. Notice that this starts the universe, which now
waits for worlds to register.  Switch to world tab and run it.  Notice
that the world successfully registered with the universe and the
universe reports that the world signed up with it.]

Thrilling.

At this point, the world registers, but never sends any message to the
server.  Now let's modify the world program to notify the server when
it ticks by sending it's current count as a message.

That requires changing our on-tick method from:

    (define/public (on-tick)
      (new counter-world% (add1 (field n))))

to one that constructs a package:

    (define/public (on-tick)
      (make-package
       (new counter-world% (add1 (field n)))
       (add1 (field n))))

Now let's re-run the world program.  Notice that messages are being
received by the server.

Now let's change the system so that "work" of adding is done on the
server side.  When the world ticks it sends its current count to the
server; when the server receives the count, it responds by sending a
message back to the world that is the count plus one.  On the world
side, that means we must now add the ability to receive messages by
implementing the @racket[on-receive] method, which will update the
world state appropriately:


  (define/public (on-tick)
    (make-package this (field n)))
  
  (define/public (on-receive m)
    (new counter-world% m))


On the server side, we now need to send a message back to the world,
so we need to consider the data definition for mail:

   A Mail is a (make-mail IWorld S-Exp).

This constructs a message that will be sent to the world represented
by the IWorld value consisting of the S-Exp value.

  (define/public (on-msg iw m)
    (make-bundle this (list (make-mail iw (add1 m))) empty)))

Notice that the universe console is now showing messages going in both
directions.

Now let's see an example of multiple world programs communicating with
a single server.

If we were to just write this:

   (big-bang (new counter-world% 0))
   (big-bang (new counter-world% 50))

the program would wait for the first big-bang expression to finish
evaluating before moving on the second one.  To make it possible to
run many worlds at the same time, the universe library provides the
@racket[launch-many-worlds] form that will evaluate all of its
subexpressions in parallel:

   (launch-many-worlds
     (big-bang (new counter-world% 0))
     (big-bang (new counter-world% 50)))

Notice that both worlds count independently.

;;;;;;;;;;;

Now let's a more interesting game.  We'll start by considering the
guess my number game.

In this game, the server is thinking of a number and you have to guess
it.

Here's the server:

#lang class1
(require class1/universe)

(define-class universe%
  (fields the-number)
  
  (define/public (on-new iw) 
    (make-bundle this empty empty))
  
  (define/public (on-msg iw m)
    (make-bundle this 
                 (list (make-mail iw (respond m (field the-number))))
                 empty)))

;; Number Number -> String
(define (respond guess number)
  (cond [(< guess number) "too small"]
        [(> guess number) "too big"]
        [else "just right"]))

(universe (new universe% 2))

The universe now has a single peice of data, which is the number it is
thinking of.  It responds to guesses by sending back a string
indicating whether the guess is just right, too big, or too small.

Here is the client:

#lang class1
(require class1/universe)
(require 2htdp/image)

(define-class guess-world%
  (fields status)
  
  (define/public (on-receive m)
    (new guess-world% m))
 
  
  (define/public (to-draw)
    (overlay (text (field status)
                   40
                   "red")
             (empty-scene 300 100)))
  
  (define/public (on-key k)
    (local [(define n (string->number k))]
      (if (number? n)
          (make-package this n)
          this)))                    
  
  (define/public (register) LOCALHOST))

(big-bang (new guess-world% "guess a number"))

The client has a single peice of data, which represents the status of
its guess.  It responds to numeric key events by sending the guess to
the server and ignores all other key events.

The @racket[string->number] function is being used to test for numeric
key events---it works by producing @racket[false] when given a string
that cannot be converted to a number, otherwise it converts the string
to a number.

;;;;;

Now let's write a 2-player version of the game where one player thinks
of a number and the other player guesses.

Here is the server:

#lang class1
(require class1/universe)

;; A Universe is a (new universe% [U #f Number] [U #f IWorld] [U #f IWorld]).
(define-class universe%
  (fields number
          picker
          guesser)
  
  ;; is the given world the picker?
  (define/public (picker? iw)
    (and (iworld? (field picker))
         (iworld=? iw (field picker))))
  
  ;; is the given world the guesser?
  (define/public (guesser? iw)
    (and (iworld? (field guesser))
         (iworld=? iw (field guesser))))
    
  (define/public (on-new iw)
    (cond [(false? (field picker))
           (make-bundle
            (new universe% false iw false)
            (list (make-mail iw "pick a number"))
            empty)]          
          [(false? (field guesser))
           (make-bundle
            (new universe% (field number) (field picker) iw)
            empty
            empty)]          
          [else
           (make-bundle this empty (list iw))]))
  
  (define/public (on-msg iw m)
    (cond [(and (picker? iw)
                (false? (field number)))           
           (make-bundle
            (new universe% m (field picker) (field guesser))
            empty
            empty)]
          [(picker? iw) ;; already picked a number
           (make-bundle this empty empty)]
          [(and (guesser? iw)
                (number? (field number)))
           (make-bundle this 
                        (list (make-mail iw (respond m (field number))))
                        empty)]
          [(guesser? iw)
           (make-bundle this
                        (list (make-mail iw "no number"))
                        empty)])))
  

(define (respond guess number)
  (cond [(< guess number) "too small"]
        [(> guess number) "too big"]
        [else "just right"]))

(universe (new universe% false false false))

The client stays the same!
