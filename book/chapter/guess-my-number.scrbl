#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/0 define-struct ... length quote))
          (for-label 2htdp/image)          
          (for-label (only-in lang/htdp-intermediate quote))
          (for-label class/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class/2))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))


@title[#:tag "guess-my-number-book"]{Guess my number}

In this chapter, we'll take an in-depth look at small, but interesting
distributed game: the ``Guess my Number'' game.


@section{One player Guess my Number}

Let's start by considering a slimmed-down version of guess my number
in which there is just one player, the client, who tries to guess the
number the server is thinking of.

@subsection{The GmN server}

In this simplified version of the game, there is not much the server
needs to do:
@itemlist[
@item{it should remember what number it is thinking of,}
@item{and it should respond to guesses made by the player.}]

Thus a natural representation of the server is an object with a single
field, which will hold the number being thought of, and an
@racket[on-msg] method that will respond to a guess made by the
player.

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
(check-expect ((new thinking-of% 7) . guess 5))
(check-expect ((new thinking-of% 7) . guess 9))
(check-expect ((new thinking-of% 7) . guess 7))
(define (guess m) ...)
}}

The final step of writing the code is trivial at this point,
so we can move on to the @racket[on-msg] method:

@filebox[@racket[thinking-of%]]{
@classblock{
;; on-msg : IWorld Any -> Result
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

Here is the client:

@codeblock{
#lang class/0
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
}

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
