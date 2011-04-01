#lang scribble/manual

@title[#:tag "lec23"]{3/28: Mixins}

@verbatim{
Today:

Announcements:

* No homework assignment out currently.  Remainder of assignments will
  be project-based.  It will be a networked game with a smart computer
  player.  More on project on Thursday.  The first assignment of the
  project will 


Today we're going to talk about a new kind of abstraction, which you
won't find in many programming languages.

Think about a representation of Shapes:

        Shape
        ^  ^
  + ----+  +----+
  |             |
Circle      Rectangle

Recall our simple encoding of these objects as functions in ASL:

(define (shape%)
 (lambda (msg)
   (cond [else (error "message not understood")])))

(define (circ% r)
  (local [(define super (shape%))]
    (lambda (msg)
      (cond [(symbol=? msg 'r) r]
            [(symbol=? msg 'height) (* 2 r)]
            [(symbol=? msg 'width) (* 2 r)]
            [(symbol=? msg 'draw) (circle r "solid" "red")]
            [else (super msg)]))))

(define (rect% h w)
  (local [(define super (shape%))]
    (lambda (msg)
      (cond [(symbol=? msg 'height) h]
            [(symbol=? msg 'width) w]
            [(symbol=? msg 'draw) (rectangle w h "solid" "red")]
            [else (super msg)]))))


But now I want to draw shapes with borders around them.

How can we add that functionality?  We can create a subclass of
rectangles that adds that functionality.

(define (rect-bb% h w)
  (local [(define super (rect% h w))]
    (lambda (msg)
      (cond [(symbol=? msg 'draw) 
             (overlay (rectangle w h "outline" "black")
                      (super 'draw))]
            [else (super msg)]))))

(define rbb (rect-bb% 400 500))
(rbb 'draw)

Now we want to do something similar for circles:

(define (circ-bb% r)
  (local [(define super (circ% r))]
    (lambda (msg)
      (cond [(symbol=? msg 'draw) 
             (overlay (rectangle w h "outline" "black")
                      (super 'draw))]
            [else (super msg)]))))

But there's a problem here -- w and h are not defined.  How can get
width and height values?  Luckily, there's a method for that, and the
super class understands it.

(define (circ-bb% r)
  (local [(define super (circ% r))]
    (lambda (msg)
      (cond [(symbol=? msg 'draw) 
             (overlay (rectangle (super 'width) (super 'height) "outline" "black")
                      (super 'draw))]
            [else (super msg)]))))

That's nice, but we duplicated a lot of code.  How can we abstract
this?  What's the first step?  Circle the things that are different.

What are the difference: circ% vs rect%, (super 'width) vs w, (super
'height) vs h, arguments to constructor are different.

Can we make them more similar?  In rectangle, we replace w with (super
'width), etc:

(define (rect-bb% h w)
  (local [(define super (rect% h w))]
    (lambda (msg)
      (cond [(symbol=? msg 'draw) 
             (overlay (rectangle (super 'width) (super 'height) "outline" "black")
                      (super 'draw))]
            [else (super msg)]))))

Now the difference are just the inputs and the calls to the super
class constructor.  So let's pull them out and make them a parameter.


(define (abstract-bb% shape)
  (local [(define super shape)]
    (lambda (msg)
      (cond [(symbol=? msg 'draw) 
             (overlay (rectangle (super 'width) (super 'height) "outline" "black")
                      (super 'draw))]
            [else (super msg)]))))

Now to recreate:

(define (rect-bb% h w) (abstract-bb% (rect% h w)))
(define (circ-bb% r)   (abstract-bb% (circ% r)))

This is a pattern called "prototypes".  You'll find this form of
abstraction in JavaScript.

We've abstracted away the super object.

There's another way we could abstract this.

We could abstract our super CLASS.  What's a class in this setting?  A
function.  So let's abstract that!

(define (m-bb% super%)
   ...)  

There's some trickiness because rect% and circ% take a different
number of arguments.  We could accept a list of arguments:

(define (m-bb% super%)
  (lambda (args)
    (local [(define super (super% args))]
      (lambda (msg)
        (cond [(symbol=? msg 'draw) 
               (overlay (rectangle (super 'width) (super 'height) "outline" "black")
                        (super 'draw))]
              [else (super msg)])))))

But this requires us to fix up how contructors work to always accept
their arguments as a list.

(define (rect% ls)
  (local [(define super (shape%))]
    (lambda (msg)
      (cond [(symbol=? msg 'height) (first h)]
            [(symbol=? msg 'width) (second w)]
            [(symbol=? msg 'draw) (rectangle w h "solid" "red")]
            [else (super msg)]))))

(define (mixin-bb% super%)
  (lambda (args)
    (local [(define super (super% args))]
      (lambda (msg)
        (cond [(symbol=? msg 'draw) 
               (overlay (rectangle (super 'width) (super 'height) "outline" "black")
                        (super 'draw))]
              [else (super msg)])))))

A Mixin is just a function from classes to sub-classes.

What's the contract for mixin-bb%?

;; Class -> Class

But what else do we need to say about the input Class?

;; [Implements width, height, draw] -> [Implements width, height, draw]

The input class needs to include the methods width, height, and draw.

We could also have implemented new methods!

Allows us to have a re-usable peice of functionality that we can apply
to multiple existing classes.

But I could have just done this by implementing the functionality in
the superclass (shape% in this example).

When can I not abstract to a super class?  When there is no common
super class.  Yet we can use mixins to do this.

Or selectively choose among different peices of functionality.

************************************************************************

Now let's see all of this in the setting of the languages we've been
working with in class.

In BSL, we had functions, but we couldn't pass them around as
arguments to other functions.

In ISL, we took away that restriction.

Up through class4, we had classes, but we couldn't pass them around as
arguments to other things.

Today we're going to take away that restriction.

You no longer have to write

(define-class shape% ...)

and can instead write

(define shape% (class ...))

Which is exactly analogous to what we've seen previously where we
revealed that:

(define (f x) ...)

is the same as:

(define f (lambda (x) ...))

So here's a very simple class:

(define shape% (class))

Now let's define some subclasses:

(define rect% 
  (class (super shape%)
    (fields height width)))

One difference that class5 forces on us.

Previously, DrRacket could figure out the basic elements of a class
when you wrote down a superclass name in the super clause.  But now
that's just an expression, and DrRacket can't figure out from a
general expression what the class, methods, and so on are.  So we're
now required to write down an interface.

Interfaces also grow a little bit:

(define-interface shape<%> () ())

What fields we know are going to be in that class (we can always have
more, but you won't be able to call anything that's not listed in the
interface).

(define rect% 
  (class (super shape% shape<%>)
    (fields height width)
    (define/public (draw)
      (rectangle (field width) (field height) "solid" "red"))))


((rect% 40 50) . draw)

So far this is looking like nothing very new.

(define circ% 
  (class (super shape% shape<%>)
    (fields radius)
    (define/public (draw)
      (circle (field radius) "solid" "red"))
    (define/public (height) (* 2 (field radius)))
    (define/public (width) (height))


Here we see an important difference between (m) and (this . m).  The
form (m) only works for methods that we know are there, i.e. they are
in the interface.  But we can always call methods, even if they
weren't explicitly listed in the interface using (this . m).

Now let's replay the abstraction process:

(define (add-bb class%)
  (class (super class% ....)  ...)

So what do we need to write down for the interface here?

(define-interface drawable<%> (draw height width) ())

This is saying something is drawable<%> if we can call the methods
draw, height, and width.

;; Class implements drawable<%> -> Class implements rect-bb method
(define (add-bb class%)
  (class (super class% drawable<%>)
     (define/public (draw-bb) 
       (overlay (rectangle (width) (height) "outline" "black")
                (draw)))))

(define rect-bb% (add-bb rect%))

((circ-bb% 40) . draw-bb)

This is abstraction over classes!!

We obtained this by loosening the restriction on naming classes and
have an expression form that just produces a class VALUE.

A bunch of features that we've seen so far map in a straightforward
way to Java.  So you might wonder, how can I do this sort of thing in
Java?  And the unfortunate answer is: you cannot.  Java only gives you
the equivalent of `define-class'.

Some languages, like Ruby, provide mixin features.

Added benefits:

- Might not be applicable to every shape.
- Might not have a common super class.
- Might not have access to the super class.  Mixin lets us add
  functionality to existing classes without changing their definition.

************************************************************************

Doors 

An adventure game where you wander through a maze.

There might be a bunch of different kinds of doors.

- TrapDoor
- LockedDoor
- GlassDoor
- HiddenDoor

But couldn't TrapDoor and GlassDoor and HiddenDoor all be locked?

What if we wanted a 

LockedDoor
  |
LockedGlassDoor
  |
LockedGlassTrapDoor
  |
HiddenLockedGlassTrapDoor

But there are so many more kinds of doors.  We don't want to enumerate
all of them.

Instead we can use a single Door class and have a mixin for each kind
of functionality we want.

(define-class door%
  (define/public (open) "It opened."))

((door%) . open)

(define-interface door<%> (open) ())

(define (trap-mixin %)
  (class (super % door<%>)
    (define/public (open) "You fell in a pit!")))

(define (lock-mixin %)
  (class (super % door<%>)
    (fields locked?)
    (define/public (unlock!) 
      (set-field! locked? false))
    (define/public (open)
      (if (field locked?)
          "Door is locked."
          (super)))))

Does the order to apply mixins matter?

(define tl-door% (trap-mixin (lock-mixin door%)))   ; open, even if locked, you fall in a pit
(define lt-door% (lock-mixin (trap-mixin door%)))   ; open, if unlocked, fall in a pit; otherwise, locked.

Not all doors are constructed the same way.

A plain trapped door:

(define t-door% (trap-mixin door%))

(t-door%)

How about a tl-door%?

((tl-door% true) . open)
((lt-door% true) . open)

Initialization is another place where the order matters since the most
recently applied mixin is gets its arguments first.


************************************************************************

One more application of mixins.

Add a bunch of functionality based on a single method.

Let's write a class for fractions:

;; A Compare is one of: '<, '>, '=.

;; A Frac is a (fract% Number Number)
(define-class fract%
  (fields num den)
  (define/public (->number) (/ (num) (den)))
   ;; Frac -> Compare
   (define/public (compare that)
     (cond [(< (->number) (that . ->number)) '<]
           [(> (->number) (that . ->number)) '>]
           [else '=])))

(check-expect ((frac% 1 2) . compare (frac% 2 4)) '=)
(check-expect ((frac% 1 2) . compare (frac% 2 5)) '>)
(check-expect ((frac% 1 2) . compare (frac% 2 3)) '<)


We could define compare for a lot of different things that might not
have a common super class.  BUT we can add a lot of useful methods
built out of a compare method and mix it in with any class that
implements compare.


(define-interface comparable<%> (compare) ())
(define-interface more-compareable<%> (compare =? <? >?))

(define (add-comparisons %)
  (class (super % comparable<%>)
    (implements more-comparable<%>)
    ;; X -> Boolean
    (define/public (=? other)
      (symbol=? '= (compare other)))
    (define/public (<? other)
      (symbol=? '< (compare other)))
    (define/public (>? other)
      (symbol=? '> (compare other)))))

Tons of classes (set, lists, strings, numbers) have a natural
ordering, but no common super classes.

A few uses:

1) Existing class hierarchy and we want to add new things without
changing the hierarchy.

2) All you have is some simple method and you build a lot of
functionality out it.

3) What we saw with doors.  We have a whole bunch of possibilities and
we can't to combine them programmatically.

All just the same the same advantages you have from other forms of
abstractions.  Mixins are just a pattern on abstracting over classes,
but rely on a feature of our class system that is not included in many
languages: classes are just values.
}