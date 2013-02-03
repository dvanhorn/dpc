#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/0 define-struct ... length quote))
          (for-label 2htdp/image)          
          (for-label (only-in lang/htdp-intermediate quote))
          (for-label class/universe))



@title[#:tag "guess-my-number-book"]{Guess my number}

In this chapter, we'll take an in-depth look at small, but interesting
distributed game: the ``Guess my Number'' game.

@section{One Player Guess my Number}

Let's start by considering a slimmed-down version of guess my number
in which there is just one player, the client, who tries to guess the
number the server is thinking of.


@subsection{The GmN server}

In this simplified version of the game, there is not much the server
needs to do:
@itemlist[
@item{it should remember what number it is thinking of,}
@item{and it should respond to guesses made by the player.}]

From the server's point of view, the interactions look like the
following, supposing the server is thinking of @racket[5]:

@verbatim{
 Client               Server
--------             ---------
                      Message
          =========>
          --------->
          <--------- "too small"
          ---------> 9
          <--------- "too big"
          ---------> 6
          <--------- "too big"
          ---------> 5
          <--------- "just right"
}

In order to respond with ``too big'', ``too small'', or ``just
right'', the state of the server will need to include the number that
the server has in mind.  Thus a natural representation of the state of
the server is an object with a single field that contains the number,
and an @racket[on-msg] method that will respond to a guess made by the
player:

@classblock{
(define-class thinking-of%
  (fields n)
  ;; on-msg : IWorld SExp -> Universe
  ;; Mail response to guess from given world
  (define (on-msg iw msg) ...))
}

In support of @racket[on-msg], let's design a method that consumes a
guess (a real number) and produces either the string
@racket["too small"], @racket["too big"], or @racket["just right"]
depending on whether the guess is smaller, bigger, or equal to the
number the server contains.

@filebox[@racket[thinking-of%]]{
@classblock{
;; A Response is one of:
;; - "too big"
;; - "too small"
;; - "just right"

;; guess : Real -> Response
;; Respond to a given guess
(check-expect ((new thinking-of% 7) . guess 5) "too small")
(check-expect ((new thinking-of% 7) . guess 9) "too big")
(check-expect ((new thinking-of% 7) . guess 7) "just right")
(define (guess m) ...)
}}

The final step of writing the code is trivial at this point,
so we can move on to the @racket[on-msg] method:

@filebox[@racket[thinking-of%]]{
@classblock{
;; on-msg : IWorld SExp -> Universe
;; Mail response to guess from given world
(check-expect ((new thinking-of% 7) . on-msg iworld1 "Bogus")
              (new thinking-of% 7))
(check-expect ((new thinking-of% 7) . on-msg iworld1 5)
              (make-bundle (new thinking-of% 7)
                           (list (make-mail iworld1 "too small"))
                           empty))
(define (on-msg iw msg) ...)
}}

Again the code is trivial once the initial design work is complete.

@subsection{The GmN Client}

The client program will register with the server and allow the user to
propose guesses which are sent to the server.  The response of ``too
small'', ``too larg'', or ``just right'' is shown to the user and they
can propose more guesses if desired.  For the moment, let's just focus
on guessing a single digit to make things simple.  We'll look at
mult-digit guesses later.  In this simplified setting the world can be
in one of two states: the client is accepting guesses, or it has a
guess and it is waiting for the server to respond to that guess.

So we arrive at the interface definitions:

@classblock{
;; A Client is one of:
;; - Waiting
;; - Accepting
;;
;; A Waiting implements:
;; - to-draw : -> Scene
;; - on-receive : SExp -> Client
;;
;; An Accepting implements:
;; - to-draw : -> Scene
;; - on-key : SExp -> Client
}

So from the client's perspective, interactions with the server will
look like the following:

@verbatim{
    Client                      Server
------------------             ---------
Event    State
Bang     Accepting  =========>
Key "3"  Waiting    ---------> 3
Msg      Accepting  <--------- "too small"
Key "9"  Waiting    ---------> 9
Msg      Accepting  <--------- "too big"
Key "6"  Waiting    ---------> 6
Msg      Accepting  <--------- "too big"
Key "5"  Waiting    ---------> 5
Msg      Accepting  <--------- "just right"
}

On further reflection, you should discover that there are in fact two
different kinds of @tt{Accepting} states the client could be in: one
in which no guess has been made---so client is waiting to accept what
will be the initial guess, and another in which a guess has been and a
response has been received from the server about that guess. In this
case, we want the client to display the guess and the server's
response while waiting for the next guess.  Based on this analysis,
it's clear we will need two implementations of @tt{Accepting} with
different behaviour and data:

@classblock{
;; A (new no-guess%) implements Accepting
(define-class no-guess%
  (define (register) ...)
  (define (to-draw) ...)
  (define (on-key ke) ...))

;; A (new waiting% Number) implements Waiting
(define-class waiting%
  (fields n)
  (define (to-draw) ...)
  (define (on-receive msg) ...))

;; A (new inform% Number String) implements Accepting
(define-class inform%
  (fields n msg)
  (define (to-draw) ...)
  (define (on-key msg) ...))
}

Here is the interactions diagram, revised slightly to be more precise
about the state of the client:

@verbatim{
      Client                             Server
----------------------------            ---------
Event    State
Bang     (new no-guess%)      =========>
Key "3"  (new waiting% 3)     ---------> 3
Msg      (new inform% 3 ...)  <--------- "too small"
Key "9"  (new waiting% 9)     ---------> 9
Msg      (new inform% 9 ...)  <--------- "too big"
...
}

First, let's fix the dimensions of the background image
and make a function for displaying strings:

@classblock{
(define MT-SCENE (empty-scene 400 400))
;; String -> Image
(define (txt str)
  (text str 40 'red))
}

The @racket[no-guess%] class represents the initial state of the
client and should display a message to the user to make a guess.  When
a key is pressed in this state, if it's numeric, that number becomes
the new guess.  Otherwise the key is ignored.  Some examples:

@filebox[@racket[no-guess%]]{
@classblock{
(check-expect ((new no-guess%) . to-draw)
              (overlay (txt "Take a guess") MT-SCENE))
(check-expect ((new no-guess%) . on-key "h")
              (new no-guess%))
(check-expect ((new no-guess%) . on-key "7")
              (make-package (new waiting% 7) 7))
}}

The remaining work of writing the code is easy:
@filebox[@racket[no-guess%]]{
@classblock{
(define (to-draw)
  (overlay (txt "Take a guess") MT-SCENE))

(define (on-key ke)
  (local [(define n (string->number ke))]
    (cond [(number? n)
           (make-package (new waiting% n) n)]
          [else this])))
}}

The @racket[string->number] function is being used to test for numeric
key events---it works by producing @racket[false] when given a string
that cannot be converted to a number, otherwise it converts the string
to a number.

The @racket[waiting%] class represents the client waiting for a
response from the server.  To render this state, let's display the
number that has been guessed.  Since this class of objects doesn't
have a @racket[on-key] event, we are implicitly disallowing further
guesses while waiting.  If the server responds with a string message,
the client transitions to a new accepting state.  Some examples:

@filebox[@racket[waiting%]]{
@classblock{
(check-expect ((new waiting% 5) . to-draw)
              (overlay (txt "Guessed: 5") MT-SCENE))
(check-expect ((new waiting% 5) . on-receive "too small")
              (new inform% 5 "too small"))
(check-expect ((new waiting% 5) . on-receive 'something)
              (new waiting% 5))
}}

All that's left is to write some code:

@filebox[@racket[waiting%]]{
@classblock{
(define (to-draw)
  (overlay (beside (txt "Guessed: ")
                   (txt (number->string (this . n))))
           MT-SCENE))

(define (on-receive msg)
  (cond [(string? msg)
         (new inform% (this . n) msg)]
        [else this]))
}}

Finally, the @racket[inform%] class represents clients that have
guessed, received a response, and are now waiting for subsequent
guesses.

By virtue of not having an @racket[on-receive] method, a client in the
accepting state will ignore message from the server (which should be
considered an error on the server's part).  Just like
@racket[no-guess%], it should accept numeric key presses as a new
guess and transition to the waiting state.  To render the state,
we should display the guess and the feedback from the server.
For example:

@classblock{
(check-expect ((new inform% 7 "too small") . to-draw)
              (overlay (txt "Guessed: 7; too small") MT-SCENE))
(check-expect ((new inform% 7 "too small") . on-key "a")
              (new inform% 7 "too small"))
(check-expect ((new inform% 7 "too small") . on-key "9")
              (make-package (new waiting% 9) 9))
}

The code is just as easy as in the other classes:

@filebox[@racket[inform%]]{
@classblock{
(define (to-draw)
  (overlay (beside (txt "Guessed: ")
                   (txt (number->string (this . n)))
                   (txt "; ")
                   (txt (this . msg)))
           MT-SCENE))

(define (on-key ke)
  (local [(define n (string->number ke))]
    (cond [(number? n)
           (make-package (new waiting% n) n)]
          [else this])))
}}

Notice that the @racket[on-key] method of @racket[inform%] and
@racket[no-guess%] are @emph{identical}.  We'll discuss how to
abstract such identical code in @secref{inheritance}.

Now we can play the game with:

@classblock{
(launch-many-worlds (big-bang (new no-guess%))
                    (universe (new thinking-of% (random 10))))
}

@subsection{Many Players, One Number}

Although we've developed this program under the simplifying assumption
that there's only one client, the server works just as well when there
are multiple clients.  Under this scenario, all of the clients are
trying to guess the one number the server is thinking of in parallel.
For example, try this out:

@classblock{
(launch-many-worlds (big-bang (new no-guess%))
                    (big-bang (new no-guess%))
                    (big-bang (new no-guess%))
                    (universe (new thinking-of% (random 10))))
}

It would take more work and a redesign of the server if we wanted to
have the server think of a number for each of the clients
independently.  We'll examine such a redesign later in the chapter,
but first, let's look at how to implement a better client.

@subsection{Guessing Big}

It's not so fun to play guess my number when the numbers can only be
between zero and nine.  But note that this limitation exists only in
the client.  The server is perfectly capable of serving up any real
number, but the client as currently designed will have a difficult
time against @racket[(new thinking-of% 11)].  The good news is that
the hard part---dealing with the protocol of messages---is behind us.
It's a small matter of iterative refinement to make the client
capable of playing larger numbers.

Looking back at our initial design, it should be clear that some of
the peices we developed can still be used.  In particular, the
@racket[waiting%] class is perfectly sufficient for dealing with
numbers larger than 9.  The problem is we have no good way of getting
to that point from @racket[no-guess%].  So let's reconsider the states
of the client.  It seems that if we want to accept multi-digit input,
we need to have a new class of @tt{Accepting} clients that has
received some digits but is ready to accept more.  We have to settle
on some input to signify the end of digits, at which point a complete
number has been given and can be shipped off to the server as a guess.

@classblock{
;; A (new continue% NumberString) implements Accepting
(define-class continue%
  (fields digits)
  (define (to-draw) ...)
  (define (on-key ke) ...))
}

The @racket[digits] field will hold a string containing all of the
digits entered so far (it will always be non-empty and can be
converted to a number with @racket[string->number]).  Let's say that
when the user presses the ``Enter'' key, the input is complete.  If
the user presses any key other than ``Enter'' or a digit, let's ignore
it.  To render a continue state, let's display ``Guessing:'' and the
digits entered so far followed by an underscore to indicate that the
client is waiting for more input.

We can now formulate some examples:

@filebox[@racket[continue%]]{
@classblock{
(check-expect ((new continue% "123") . to-draw)
              (overlay "Guessing: 123_" MT-SCENE))
(check-expect ((new continue% "123") . on-key "4")
              (new continue% "1234"))
(check-expect ((new continue% "123") . on-key "a")
              (new continue% "123"))
(check-expect ((new continue% "123") . on-key "\r")
              (make-package (new waiting% 123) 123))
}}

Now for the code:

@filebox[@racket[continue%]]{
@classblock{
(define (to-draw)
  (overlay (beside (txt "Guessing: ")
                   (txt (this . digits))
                   (txt "_"))
           MT-SCENE))

(define (on-key ke)
  (cond [(number? (string->number ke))
         (new continue% (string-append (this . digits) ke))]
        [(key=? "\r" ke)
         (local [(define n (string->number (this . digits)))]
           (make-package (new waiting% n) n))]
        [else this]))
}}

We now need to go back and revise @racket[no-guess%] and
@racket[inform%] to transition to @racket[continue%] whenever a digit
key is pressed:

@classblock{
(check-expect ((new no-guess%) . on-key "7")
              (new continue% "7"))
(check-expect ((new inform% 5 "too small") . on-key "7")
              (new continue% "7"))
}

The code in both cases is:

@classblock{
(define (on-key ke)
  (cond [(number? (string->number ke))
         (new continue% ke)]
        [else this]))
}

Now try this out:

@classblock{
(launch-many-worlds
  (big-bang (new no-guess%))
  (universe (new thinking-of% (random 1000))))
}


@section{Two player guess my number}

[FIXME this section is out of sync with previous sections and needs
to be re-written.]

Now let's write a 2-player version of the game where one player thinks
of a number and the other player guesses.

Here is the server:

@codeblock{
#lang class/0
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
}

The client stays the same!  You can launch the two players with:

@classblock{
(launch-many-worlds
 (big-bang (new guess-world% "guess a number"))
 (big-bang (new guess-world% "guess a number")))
}
