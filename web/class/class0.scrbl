#lang scribble/manual

@require[(for-label class0/universe
		    class0
		    (prefix-in 2htdp: 2htdp/universe)
		    (except-in 2htdp/image image?))]

@title[#:style 'quiet]{Class 0}

@defmodulelang[class0]

@defform[(require module-name ...)]{
Imports all the modules named @racket[module-name]s.}

@defform/subs[#:literals (fields define/public define/private)
	      (define-class class-name 
                 fields-spec
		 method-spec ...)
	      ([fields-spec code:blank
			    (fields field-name ...)]
	       [method-spec (define/public (method-name arg ...)
			      body)
			    (define/private (method-name arg ...)
			      body)])]{

Defines a new class named @racket[class-name] with fields
@racket[field-name]s and methods @racket[method-name]s.
The class has one additional method for each field name
@racket[field-name].

Methods defined with @racket[define/public] are accessible both inside
and outside of the class definition, while methods defined with
@racket[define/private] are only accessible within the class
definition.

To refer to a field within the class definition, use @racket[(field
field-name)].

Methods may be invoked within the class definition using the function
call syntax @racket[(method-name arg #,(racketidfont "..."))], but
must be invoked with @racket[send] from oustide the class definition
as in @racket[(send object method-name arg #,(racketidfont "..."))].

The name @racket[this] is implicitly bound to the current object,
i.e. the object whose method was called.

To construct an instance of @racket[class-name], use @racket[(new
class-name arg #,(racketidfont "..."))] with as many arguments as there are fields in the
class.}

@deftogether[
[@defidform[this]
 @defform[(fields id ...)]
 @defform[(define/public (method-name id ...) body)]
 @defform[(define/private (method-name id ...) body)]]]{
See @racket[define-class].}

@defform[(new class-name arg ...)]{Creates an object which is an instance of
@racket[class-name] with @racket[arg]s as the values for the fields.}

@defform[(field field-name)]{References the value of the field named
@racket[field-name].}

@defform[(send object message arg ...)]{ 

Send @racket[object] the message @racket[message] with arguments
@racket[arg]s, that is, invoke @racket[object]'s @racket[message]
method with the given arguments, @racket[arg]s.}

@section[#:style 'hidden]{Object-oriented Universe}

@defmodule[class0/universe #:use-sources ("class/tick-rate.rkt")]

@defproc[(big-bang [obj World]) World]{

An object-oriented version of @racket[2htdp:big-bang].

The given world object should provide some of the methods descibed
below.  For any methods that are not provided, DrRacket will use a
default behavior.  The world @emph{must} at least provide a
@racket[to-draw] method.

Here is an example world program that counts up from zero:

@racketmod[
class0
(require class0/universe)
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

@require[(for-label "tick-rate.rkt")]

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
