#lang scribble/manual
@(require #;"unnumbered.rkt"
          "utils.rkt")

@title[#:style 'hidden]{Object-oriented Universe}


@(require
  (for-label "universe.rkt" 
  class/0   
  (prefix-in 2htdp: 2htdp/universe)
  (except-in 2htdp/image image?)))
@defmodule[class/universe]

@(define-syntax-rule (def-racket big-bang-id universe-id)
  (begin
    (require (for-label (only-in 2htdp/universe big-bang universe)))
    (define big-bang-id (racket big-bang))
    (define universe-id (racket universe))))
@(def-racket big-bang-id universe-id)

@section[#:style 'hidden #:tag-prefix 'big-bang]{Big bang}

@defproc[(big-bang [obj World]) World]{

An object-oriented version of @|big-bang-id|.

The given world object should provide some of the methods descibed
below.  For any methods that are not provided, DrRacket will use a
default behavior.  The world @emph{must} at least provide a
@racket[to-draw] method.

Here is an example world program that counts up from zero:

@racketmod[
class/0
(require class/universe)
(require 2htdp/image)

(define-class counter-world%
  (fields n)
  
  (define (on-tick)
    (new counter-world% (add1 (send this n))))
  
  (define (tick-rate)
    1)
  
  (define (to-draw)
    (overlay (text (number->string (send this n))
		   40
		   "red")
	     (empty-scene 300 100))))

(big-bang (new counter-world% 0))
]}

@defform[#:id name
         #:literals (send name)
  (send a-world name)]{

  Produces the name of the world.}

@defform[#:id on-tick 
	 #:literals (send on-tick) 
  (send a-world on-tick)]{

  Tick this world, producing a world.}

@defform[#:id on-key
	 #:literals (send on-key) (send a-world on-key key)
         #:contracts ([key key-event?])]{

  Handle the keystroke @racket[key], producing a world.}

@defform[#:id on-release
	 #:literals (send on-release) 
         (send a-world on-release key)
         #:contracts ([key key-event?])]{

  Handle the key release @racket[key], producing a world.}

@defform[#:id on-mouse
	 #:literals (send on-mouse)
  (send a-world on-mouse x y m)
  #:contracts [(x integer?) 
               (y integer?)
               (m mouse-event?)]]{
  Handle the mouse event @racket[m] at location  (@racket[x],@racket[y]),
  producing a world.}


@defform[#:id to-draw
	 #:literals (send to-draw)
  (send a-world to-draw)]{
  
  Draw this world, producing an image.}


@defform[#:id tick-rate
	 #:literals (send tick-rate)
  (send a-world tick-rate)]{

  Produce the rate at which the clock should tick for this world.}


@defform[#:id stop-when
	 #:literals (send stop-when)
  (send a-world stop-when)]{
  
  If this method produces @racket[true], the world program is shut down.}

@defform[#:id check-with
	 #:literals (send check-with)
  (send a-world check-with)]{
  
  If this method produces @racket[true], the world is considered a 
  valid world; otherwise the world program signals an error.}

@defform[#:id record?
	 #:literals (send record?)
  (send a-world record?)]{
  
  If this method produces @racket[true] for the initial world, 
  DrRacket enables a visual replay of the interaction.}

@defform[#:id state
	 #:literals (send state)
  (send a-world state)]{
  
  If this method produces @racket[true] for the initial world,
  DrRacket displays a seperate window in which the current state is
  rendered.}


@section[#:style 'hidden #:tag-prefix 'universe]{Universe}

@defproc[(universe [obj Universe]) Universe]{

An object-oriented version of @|universe-id|.}

The given universe object should provide some of the methods descibed
below.  For any methods that are not provided, DrRacket will use a
default behavior.  The universe @emph{must} at least provide a
@racket[on-new] and @racket[on-msg] method.

@defform[#:id on-new
	 #:literals (send on-new) 
  (send a-universe on-new iw)
  #:contracts [(iw iworld?)]]{

  Signal that the given new world has joined the universe, producing a bundle.}

@defform[#:id on-msg
	 #:literals (send on-msg) 	 
  (send a-universe on-msg iw msg)
  #:contracts [(iw iworld?)
               (msg sexp?)]]{

  Signal that the given world has sent the given message to the
  universe, producing a bundle.}

@defform/none[;#:id on-tick 
	 #:literals (send on-tick) 
  (send a-universe on-tick)]{

  Tick this universe, producing a bundle.}

@defform/none[;#:id tick-rate
	 #:literals (send tick-rate)
  (send a-universe tick-rate)]{

  Produce the rate at which the clock should tick for this universe.}

@defform[#:id on-disconnect
	 #:literals (send on-disconnect) 
  (send a-universe on-disconnect iw)
  #:contracts [(iw iworld?)]]{

  Signal that the given world has left the universe, producing a bundle.}

@defform/none[;#:id check-with
	 #:literals (send check-with)
  (send a-universe check-with)]{
  
  If this method produces @racket[true], the universe is considered a 
  valid universe; otherwise the universe program signals an error.}

@defform[#:id to-string
	 #:literals (send to-string) 
  (send a-universe to-string)]{

  Produce a string representation of the universe.}

@defform/none[;#:id state
	 #:literals (send state)
  (send a-universe state)]{
  
  If this method produces @racket[true] for the initial universe,
  DrRacket displays a seperate window in which the current state is
  rendered.}
