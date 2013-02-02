#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/0 define-struct ... length quote))
          (for-label 2htdp/image)          
          (for-label (only-in lang/htdp-intermediate quote))
          (for-label class/universe))

@title{Universe}

In this chapter, we're going to start looking at the design of
multiple, concurrently running programs that communicate we each
other.  We will use the @racket[universe] system as our library for
communicating programs.

@section{A look at the Universe API}

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

@classblock{
;; A Package is a (make-package World SExp).
}

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

@codeblock{
#lang class/0
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
}

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

We can now revise the @racket[on-tick] method to not only produce the
new state of the world, but additionally a message to be sent to the
server:

@filebox[@racket[cw%]]{
@classblock{
(define (on-tick)
  (make-package (new cw% (add1 (send this n)))
                (add1 (send this n))))
}}

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
@classblock{
;; register : -> String
;; IP address of server
(define (register) LOCALHOST)
}}

Now when you run this program you will see the world program try to
connect to the universe, but since we have not written---much less
run---the server, it cannot find the universe.  After a few tries, it
gives up and continues running without communicating with the
universe.

Our complete client is:

@codeblock{
#lang class/0
(require class/universe)
(require 2htdp/image)

;; Scene is 300x100 pixels
(define WIDTH 300)
(define HEIGHT 100)

;; A CounterWorld is a (new cw% Natural)
;; and implements
;; - register : -> String
;;   IP address of server
;; - tick-rate : -> Number
;;   Tick rate for counting.
;; - on-tick : -> (make-package CounterWorld Number)
;;   Increment counter world state, broadcast to server.
;; - to-draw : -> Scene
;;   View counter world state as a scene.
(define-class cw%
  (fields n)
  (define (register) LOCALHOST)
  (define (tick-rate) 1)
  (define (on-tick)
    (make-package (new cw% (add1 (send this n)))
                  (add1 (send this n))))
  (define (to-draw)
    (overlay (text (number->string (send this n)) 40 "red")
             (empty-scene WIDTH HEIGHT))))
  
;; Run, program, run!
(big-bang (new cw% 0))
}


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

This is equivalent to doing the following:

@codeblock{
#lang class/0
(require class/universe)

(define-class cu%
  ;; IWorld -> Bundle
  (define (on-new iw) this)
  
  ;; IWorld S-Expr -> Bundle
  (define (on-msg iw m) this))
}

When a world registers, the @racket[on-new] method is called with an
@tt{IWorld} value.  An @tt{IWorld} value opaquely represents a world,
that is you do not have the ability to examine the contents of the
value, but you can compare it for equality with other @tt{IWorld}
values using @racket[iworld=?].

When a world sends message, the @racket[on-msg] method is called with
the IWorld representing the world that sent the message and the S-Exp
message that was sent.

In both cases, the method must either produce a universe or a bundle:

@classblock{
;; A Bundle is a (make-bundle Universe [Listof Mail] [Listof IWorld]).
;; A Mail is a (make-mail IWorld S-Exp).
}

A bundle signals communication to some set of worlds.  Within a bundle,
the universe component is the new state of the universe; the list of
mail is a list of messages that will be sent back to the worlds (more
on this in a moment), and the list of worlds are worlds that the
server has chosen to disconnect from.

For the purposes of our example, the universe maintains no state (the
class has no data).  When a new world registers, we do nothing, and
when a world sends a message, we also do nothing, send nothing in
response, and disconnect no worlds.

Running this program launches the universe server, making it ready to
receive registrations from worlds.  After starting the universe
server, if we switch back to the world program tab and run it, we'll
see that it successfully registers with the universe and the universe
console reports that the world signed up with it.

If we examine the server side of the diagram considered above, we
see:

@verbatim{
 Client                       Server
--------             -----------------------
                     Message   Event   State
                               Bang    (new cu%)
         =========>            Join    (new cu%)
         --------->  1         OnMsg   (new cu%)
         --------->  2         OnMsg   (new cu%)
         --------->  3         OnMsg   (new cu%)
                     ...       ...     ...    
}

From the perspective of the server, the client is opaque---all we can
observe is the messages sent from the client.

The server, as written, will actually work if more than one client
connect to the server.  To try it out, just run two clients at the
same time.  When multiple clients connect, we may see interaction like
this:

@verbatim{
  A       B                   Server
-----   -----        -----------------------
                     Message   Event   State
                               Bang    (new cu%)
         =========>            Join    (new cu%)
         --------->  1         OnMsg   (new cu%)
         --------->  2         OnMsg   (new cu%)
  ================>            Join    (new cu%)
  ---------------->  1         OnMsg   (new cu%)
  ---------------->  2         OnMsg   (new cu%)
         --------->  3         OnMsg   (new cu%)
  ---------------->  3         OnMsg   (new cu%)
                     ...       ...     ...    
}

If we would like to rule out this kind of interaction and have the
server listen only to one client, that's easy to do.  The idea is that
we can develop a server that accepts no new clients.  Such a server
would have the following @racket[on-new] method:

@classblock{
(define (on-new iw)
  (make-bundle this empty (list iw)))
}

Now we can have the server start in the accepting universe state that
will allow a client to join, but as soon as one does, it transitions
to the unaccepting state that rejects all new clients:

@codeblock{
#lang class/0
(require class/universe)

;; A Server is one of:
;; - (new accept%)
;; - (new reject%)

(define-class accept%
  (define (on-new iw)
    (new reject%)))

(define-class reject%
  (define (on-new iw)
    (make-bundle this empty (list iw))))

;; Run, server, run!
(universe (new accept%))
}

The interaction described above would now look like:

@verbatim{
  A       B                   Server
-----   -----        -----------------------
                     Message   Event   State
                               Bang    (new accept%)
         =========>            Join    (new reject%)
         --------->  1         OnMsg   (new reject%)
         --------->  2         OnMsg   (new reject%)
  ================>            Join    (new reject%)
  <================            Disconn (new reject%)
         --------->  3         OnMsg   (new reject%)
                     ...       ...     ...    
}

We can imagine more sophisticated scenerios such as one where we want
to enable peer-to-peer communication.  As a simple example, let's
build a system where one client can broadcast messages to another.
This server will have to involve some data since it needs to remember
who to broadcast mail to when it receives a message.  The initial
state of the server is waiting for the broadcaster to join, after
which it waits for the broadcastee.  Once both parties have joined,
every time the broadcaster sends a message, the server relays it to
the broadcastee:

@codeblock{
#lang class/0
(require class/universe)

;; A Server is one of:
;; - (new wait-caster%)
;; - (new wait-castee%)
;; - (new relay% IWorld)

(define-class wait-caster%
  (define (on-new iw)
    (new wait-castee%)))

(define-class wait-castee%
  (define (on-new iw)
    (new relay% iw)))

(define-class relay%
  (fields castee)
  (define (on-new iw)
    (make-bundle this empty (list iw)))
  (define (on-msg iw msg)
    (make-bundle this (make-mail (send this castee) msg) empty)))

;; Run, server, run!
(universe (new wait-caster%))
}

Notice how the @racket[relay%] class contains an @tt{IWorld} that it
broadcasts to when it receives a message.  It also rejects any new
clients that try to connect. 

An example interaction for this serve is:

@verbatim{
  A       B                   Server
-----   -----        -----------------------
                     Message   Event   State
                               Bang    (new wait-caster%)
         =========>            Join    (new wait-castee%)
         --------->  1         OnMsg   (new wait-castee%)
         --------->  2         OnMsg   (new wait-castee%)
  ================>            Join    (new relay% A)
         --------->  3         OnMsg   (new relay% A)
  <----------------  3
         --------->  4         OnMsg   (new relay% A)
  <----------------  4
                     ...       ...     ... 
}

Notice how in this example, @tt{B}, the broadcaster, starts sending
messages before another client has joined.  Those messages are simply
ignored.  After a second client joins and the server goes into the
relay state, subsequent messages will be sent to @tt{A}, the
broadcastee.

@section{Simple world, receiving messages from the server}

@margin-note{This should be re-written to use the state pattern.}

In the example considered so far, the server sends messages to a
client, but that requires the client is willing to receive such
messages.  Let's look at how to adapt our simple broadcasting client
to one that listens for message from the universe server.

This client is in one of two states: it either hasn't received a
message yet, so it doesn't know what the current ``count'' is yet, or
it has received at least one message and so it knows the most recent
count, relayed from the counting to world to the server to this
client.  The listener client doesn't need to generate any of its own
events so we drop the @racket[on-tick] handler and instead add a
@racket[on-receive] method that handles incoming messages from the
server.

@codeblock{
#lang class/0
(require class/universe)
(require 2htdp/image)

;; Scene is 300x100 pixels
(define WIDTH 300)
(define HEIGHT 100)

;; A ListenerWorld is one of:
;; - (new wait-first%)         Interp: waiting for first msg.
;; - (new wait-next% Number)   Interp: waiting for next msg.
;; and implements
;; - to-draw : -> Scene
;;   View counter world state as a scene.
;; - on-receive : Number -> ListenerWorld
;;   Receive new counter state from the universe.

(define-class wait-first%
  (define (register) LOCALHOST)
  (define (on-receive msg) (new wait-next% msg))
  (define (to-draw)
    (overlay (text "Waiting" 40 "red")
             (empty-scene WIDTH HEIGHT))))
  
(define-class wait-next%
  (fields n)
  (define (on-receive msg) (new wait-next% msg))
  (define (to-draw)
    (overlay (text (number->string (send this n)) 40 "red")
             (empty-scene WIDTH HEIGHT))))

;; Run, program, run!
(big-bang (new wait-first%))
}

We can now run the clients and server and observe the communication,
but to do so, you have to be careful about the order in which you
start things.  Our server @emph{assumes} the first client to connect
is the broadcaster.  This is really a flaw in our design, and we can
look at eliminating it, but as a work around just be sure to first
start the server, then the counter world, and then the listener world.

@section{Rules of engagement: protocols and enforcement}

There are a couple subtle points worth noting about the server and the
possible interactions it allows.

First, if the broadcaster and broadcastee don't join in that order,
nothing works as intended.  While it may be reasonable to think we can
control the order for simple experiments, when our programs are
released into the wild and we have @emph{no control} over when and
where the clients run, this isn't a reasonable assumption to make.  We
should refine our program to allow the clients to join in any order.

If the clients can join in any order, a client must identify their
role by sending a message after joining.  But now the protocol, even
for this simple scenario, becomes fairly complicated.  You can imagine
a client joins but waits a long time to identify their role.  Another
client may join in the interim and immediately identify their role.
What if two clients want to broadcast?  What if two clients want to
listen?

Second, if the broadcastee ever decides to send a message, rather than just
passively listening to what's sent its way, we will treat this message
as though it came from the broadcaster (and thus send it back to the
broadcastee).  That is, the following scenario is possible:

@verbatim{
  A       B                   Server
-----   -----        -----------------------
                     Message   Event   State
                               Bang    (new wait-caster%)
         =========>            Join    (new wait-castee%)
  ================>            Join    (new relay% A)
         --------->  1         Msg     (new relay% A)
  <----------------  1
         --------->  2         Msg     (new relay% A)
  <----------------  2
  ---------------->  7         Msg     (new relay% A)
  <----------------  7
                     ...       ...     ... 
}

This brings up the interesting issue of @emph{protocols}, that is,
what are the proper rules of conduct for the clients' and server's
interaction.  First and foremost, it's important to establish and
document the rules of interaction.  After doing so, we may want to
@emph{enforce} protocols, either on the client or server side, or
both.

Let's try to develop broadcast and listener clients and a server that
address these two issues.  In this design, the clients' changes are
relatively straightforward.  They start by sending an initial message
identifying their role, either @racket['broadcast] or
@racket['listen].  

The server is more involved.  Here we model @emph{connections} with
worlds as either being an unknown connection, a broadcast connection,
or a broadcastee connection.  The server starts in a state with no
connections.  When a world joins, it transitions to a single, unknown
connection state.  When a second world joins, it transitions to a
two-connection state.  Any time after a connection has been made the
server accepts role messages and sends them to the connections.  If
the connection is in an unknown state and the role came from that
connections' underlying world, the connection is assigned the
requested role.  The only issue remaining with this server is handling
the case of two clients want to both broadcast or both listen, which
we just ignore for now.

@codeblock{
#lang class/0
;; A Server is 
;; - (new wait%)
;; - (new one% Conn)
;; - (new two% Conn Conn)

(define-class wait%
  (define (on-new iw)
    (new one% (new unknown% iw))))

(define-class one%
  (fields conn)
  (define (on-new iw)
    (new two% (send this conn) (new unknown% iw)))
  (define (on-msg iw msg)
    (cond [(role? msg)
           (new one% (send (send this conn) ident msg))]
          [else this])))

(define-class two%
  (fields conn1 conn2)
  (define (on-msg iw msg)
    (cond [(role? msg)
           (new two% 
                (send (send this conn1) ident msg)
                (send (send this conn2) ident msg))]
          [else
           (make-bundle this
                        (append (send (send this conn1) mail msg)
                                (send (send this conn2) mail msg))
                        empty)])))             

;; A Conn is one of:
;; - (new unknown% IWorld)
;; - (new caster% IWorld)
;; - (new castee% IWorld)
;; implements
;; - mail : Msg -> [Maybe Mail]
;;   Maybe send mail to this connection.
;; - ident : IWorld Role -> Conn
;;   Assign given role for this connection.

;; A [Maybe X] is one of:
;; - empty
;; - (list X)

;; An Role is one of 'broadcast or 'listen
(define (role? x) (or (eq? 'broadcast) (eq? 'listen)))

(define-class unknown%
  (fields iw)
  (define (mail msg) empty)
  (define (ident iw role)
    (cond [(iworld=? (send this iw) iw)
           (cond [(eq? role 'broadcast)
                  (new caster% (send this iw))]
                 [else
                  (new castee% (send this iw))])]
          [else this])))

(define-class caster%
  (fields iw)
  (define (mail msg) empty)
  (define (ident iw role) this))

(define-class castee%
  (fields iw)
  (define (mail msg)
    (list (make-mail (send this iw) msg)))
  (define (ident iw role) this))
}

Now that we've seen the basics of communicating programs, let's build
something a little more substantial.

@include-section{06/exercises.scrbl}