#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct check-expect ...))
          (for-label (except-in class/0 define-struct ... length check-expect))
          (for-label 2htdp/image)
          (for-label class/universe))

@(require scribble/eval racket/sandbox)
@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require (for-syntax racket/base)))
    (the-eval '(require class/0))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@title{Larger system design: Snakes on a plane}

So far, we have introduced a lot of important new concepts such as
interfaces and data and method inheritance for class-based
abstraction.  In this chapter, we are going to see these concepts
being applied in the context of a larger program design.  It's a game
we've all seen and designed before; we're going to develop a
class-based version of the Snake Game.

@section{Information in the Snake Game}

Our first task in designing the Snake Game is to take an account of
the information that our program will need to represent.  This
includes:

@itemlist[
 @item{a system of coordinates}
 @item{a snake, which has
   @itemlist[
     @item{direction}
     @item{segments}]}
 @item{food}
 @item{a world}]

@section{The world}

Let's start by designing a minimal @racket[world%] class.  We will
iteratively refine it later to add more an more features.  For now,
let's just have the snake move.

@classblock{
;; A World is a (new world% Snake Food).
(define-class world%
  (fields snake food)

  (define (on-tick)
    (new world% 
         (send (send this snake) move)
         (send this food)))

  (define (tick-rate) 1/8)

  (define (to-draw)
    (send (send this food) draw
          (send (send this snake) draw MT-SCENE))))
}

@section{Coordinate interface}

Let's focus on the system of coordinates. There are really two
coordinate systems we will need to represent; one is necessitated by
the animation system we are using, the other by the logic of the Snake
Game.  We are @emph{consumers} of the @racket[big-bang] animation
system, which uses a pixel-based, graphics-coordinate system (meaning
the origin is at the Northwest corner).  This is part of
@racket[big-bang]'s interface, which we don't have the power to
change, and therefore we have to communicate to @racket[big-bang]
using pixels in graphics-coordinates.  In general, when we design
programs, the interfaces of libraries we use impose obligations on our
code.

On the other hand, using pixel-based graphics-coordinates is probably
not the best representation choice for the information of the Snake
Game.  We'll be better off if we design our own representation and
have our program translate between representations at the
communication boundaries between our code and @racket[big-bang].
Let's use a grid-based coordinate system where the origin is the
Southwest corner.  We can define a mapping between the coordinate
systems by defining some constants such as the grid size and the 
size of the screen:

@codeblock{
#lang class/0
(define WIDTH  32) ; in grid units
(define HEIGHT 32) ; in grid units
(define SIZE   16) ; in pixels / grid unit
(define WIDTH-PX  (* SIZE WIDTH))  ; in pixels
(define HEIGHT-PX (* SIZE HEIGHT)) ; in pixels
}

This defines that our game is logically played on a 32x32 grid, which we
will render visually as a 512x512 pixel image.  This are the values we
cooked up in class, which results in the following grid for our game:

@margin-note{As an exercise, try to write an expression that produces
this image.}

@(the-eval 
  '(begin
     (require 2htdp/image)
     (require (only-in racket for*/fold in-range))
     
     (define WIDTH  32) ; in grid units
     (define HEIGHT 32) ; in grid units
     (define SIZE   16) ; in pixels / grid unit
     (define WIDTH-PX  (* SIZE WIDTH))  ; in pixels
     (define HEIGHT-PX (* SIZE HEIGHT)) ; in pixels
     
     (define (grid->px-x x)
       (* SIZE (+ 1/2 x)))
     
     (define (grid->px-y y)
       (- HEIGHT-PX (* SIZE (+ 1/2 y))))
     
     (check-expect (grid->px-x 0) (* 1/2 SIZE))
     (check-expect (grid->px-y 0) (- HEIGHT-PX (* 1/2 SIZE)))
     
     (define (add-sq x y scn)
       (place-image (overlay (text (string-append (number->string x)
                                                  ","
                                                  (number->string y))
                                   6
                                   "black")
                             (square SIZE "outline" "black"))
                    (grid->px-x x)
                    (grid->px-y y)
                    scn))
     
     (define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
     
     (for*/fold ([scn MT-SCENE])
       ([i (in-range 0 WIDTH)]
        [j (in-range 0 HEIGHT)])
       (add-sq i j scn))))

But for the sake of our notes, let's develop the game for a much
smaller grid that is rendered on a larger scale.  It will be easy to
change our definitions at the end in order to recover the original
design.  If done properly, all of test cases will remain unaffected by
the change.

@classblock{
(define WIDTH   8) ; in grid units
(define HEIGHT  8) ; in grid units
(define SIZE   32) ; in pixels / grid unit
}

This defines that our game is logically played on a 8x8 grid, which we
will render visually as a 256x256 pixel image.  The grid for our game
now looks like:

@(the-eval 
  '(begin     
     (define WIDTH   8) ; in grid units
     (define HEIGHT  8) ; in grid units
     (define SIZE   32) ; in pixels / grid unit
     (define WIDTH-PX  (* SIZE WIDTH))  ; in pixels
     (define HEIGHT-PX (* SIZE HEIGHT)) ; in pixels

     (define (grid->px-x x)
       (* SIZE (+ 1/2 x)))     
     (define (grid->px-y y)
       (- HEIGHT-PX (* SIZE (+ 1/2 y))))     
     (define (add-sq x y scn)
       (place-image (overlay (text (string-append (number->string x)
                                                  ","
                                                  (number->string y))
                                   12
                                   "black")
                             (square SIZE "outline" "black"))
                    (grid->px-x x)
                    (grid->px-y y)
                    scn))
     
     (define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
     
     (for*/fold ([scn MT-SCENE])
       ([i (in-range 0 WIDTH)]
        [j (in-range 0 HEIGHT)])
       (add-sq i j scn))))

Now we need to consider the interface for coordinates (henceforth, the
term "coordinate" refers to @emph{our} representation of coordinates
in the Snake Game, not the graphics-coordinates of @racket[big-bang]).
What do we need to do with coordinates?  Here's a coarse first
approximation:

@itemlist[
@item{Compare two coordinates for equality.}
@;item{Convert to pixel-based graphics-coordinates.}
@item{Draw something at a coordinate on to a scene.}
@item{Move a coordinate.}
@item{Determine whether a coordinate is on the board.}
]

This list suggest the following interface for coordinates:

@classblock{
;; A Coord implements

;; same-pos? : Coord -> Boolean
;; Is this coordinate at the same position as the given one?

;; draw : Scene -> Scene
;; Draw this coordinate on the scene.

;; move : -> Coord
;; Move this coordinate.

;; on-board? : -> Boolean
;; Is this coordinate on the board?
}

This is a good place to start, but as we start thinking about
@emph{what} these methods should do and @emph{how} we might write
them, some issues should come to mind.  For example, in thinking about
the what: what should be drawn when the @racket[draw] method is
invoked?  Perhaps we want the coordinate "to just know" what should be
drawn, which suggests that when we implement coordinates they should
contain data representing what to draw.  Perhaps we want to tell the
@racket[draw] method what to draw, which suggests we should revise the
contract to include an argument or arguments that represent what to
draw.  For the time-being, let's decide that the coordinate will know
what to draw.

In the thinking about the how: how do you imagine the draw coordinate
will be written?  Assuming we know what to draw, the next question is
how will the method know where to draw it?  We have a coordinate,
which is a grid coordinate, but will need to use @racket[place-image]
to actually draw that image on the given scene.  But
@racket[place-image] works in the pixel-based graphics-coordinate
system.  We need to be able to convert a coordinate to a pixel-based
graphics-coordinate in order to write the @racket[draw] method, but
there is nothing in the interface that gives us that capability,
@emph{and the interface is all we will have to work with}.  This
suggests we should revise the interface to include this needed
behavior.

Similarly, if we consider how to write the @racket[same-pos?] method,
we will want to compare the @emph{x}- and @emph{y}-components of the
given coordinate with the @emph{x}- and @emph{y}-components of this
coordinate.  Again, there is nothing in the interface as given that
allows this, so we need to revise.

Now consider the @racket[move] method.  How can we write it?  What do
we expect to happen?  There's not enough information to know---should
the coordinate move up?  Down?  Right three and down seven?  Hard to
say.  While we might expect a coordinate to know how to draw itself,
we cannot expect a coordinate to know which way to move itself.  This
suggests we need to add inputs to the method that represent this
needed information.  For the purposes of our game, a position needs to
be able to move one grid unit in one of four directions.  Let's design
the representation of a direction and a directional input to the
@racket[move] method.

Interface design is incredibly important, especially when, unlike in
our current situation, it is not easy to revise in the future.  Modern
computer systems are littered with detritus of past interface design
choices because interfaces are difficult and expensive, if not
impossible, to change.  As an example, the developers of the UNIX
operating system, which was developed in the 1970s and is now the
basis of both Linux and Mac OS, made the choice to save characters and
call the operation that creates a file "CREAT".  Forty years later,
I'm writing these notes on a portable computer while flying from
Boston to Houston.  My machine, which weighs far less than any
computer that ran UNIX in the 70's, has not one, but two 2.66 GHz
processors and 8 gigs of RAM: unimaginable computing resources in the
70s.  And yet, when my OS wants to make a new file, it calls the
"CREAT" function---not because that missing "E" is a computational
extravangance I cannot afford, far from it, but because it is simply
too difficult a task to realize a redesign of the interface between my
computer and its operating system.  The unforunate thing about
interfaces is that when they change, all parties that have agreed to
that interface must change as well.  Too many people, programs, and
devices have agreed to the UNIX interface to make changing "CREAT"
to "CREATE" worthwhile.

You won't be able to make the perfect interface on the first try, but
the closer you get, the better your life will be in the future.

Revising our interface as described above, we arrive at the following:

@classblock{
;; A Coord implements:
 
;; same-pos? : Coord -> Boolean
;; Is this coordinate at the same position as the given one?

;; draw : Scene -> Scene
;; Draw this coordinate on the scene.

;; move : Dir -> Coord
;; Move this coordinate in the given direction.

;; on-board? : -> Boolean
;; Is this coordinate on the board?

;; {x,y} : -> Nat
;; The {x,y}-component of grid-coordinate.

;; {x-px,y-px} : -> Nat
;; The {x,y}-component of pixel-graphics-coordinate.
}

We haven't designed @tt{Dir} data definition for representing
direction; let's take care of that quickly.  In our game, a direction
is one of four possibilities, i.e. it is an enumeration.  We could use
a class-based enumeration, but for the sake of simplicity, let's 
just use strings and say that:

@classblock{
;; A Dir is one of:
;; - "left"
;; - "right"
;; - "up"
;; - "down"
}

This representation has the nice property of being a subset of
@racket[big-bang]'s @tt{KeyEvent} representation, so we can rely on
the coincidence and handle the "up" key event by moving in the "up"
direction without need to convert between representations.

@section{An implementation of coordinates: segments}

At this point, we've flushed out enough of the initial design of the
coordinate interface we can now start working on an implementation of
it.  There are two components that will implement the coordinate
interface: segments and food.  Let's start with segments.

@classblock{
;; A (new seg% Int Int) is a Coord
;; Interp: represents a segment grid-coordinate.
(define-class seg%
  (fields x y)
  ...)
}

Our template for @racket[seg%] methods is:

@classblock{
;; ? ... -> ?
(define (seg-template ...)
  (... (send this x) ... (send this y) ...))
}

We've now made a data definition for segments and committed ourselves
to implementing the interface.  This obligates us to implement all of
the methods in @racket[coord%].  We've decided to implement the
@racket[coord%] using a class with an @racket[x] and @racket[y] field.
This satisfies part of our implementation right off the bat: we get an
@racket[x] and @racket[y] method by definition.  Let's now do
@racket[same-pos?]:

@filebox[@r[seg%]]{
@classblock{
(check-expect (send (new seg% 0 0) same-pos? (new seg% 0 0)) true)
(check-expect (send (new seg% 0 0) same-pos? (new seg% 1 0)) false)
(define (same-pos? c)
  (and (= (send this x) (send c x))
       (= (send this y) (send c y))))
}}

And now @racket[draw]:

@filebox[@r[seg%]]{
@classblock{
(check-expect (send (new seg% 0 0) draw MT-SCENE)
              (place-image (square SIZE "solid" "red")
                           (* 1/2 SIZE)
                           (- HEIGHT-PX (* 1/2 SIZE))
                           MT-SCENE))
(define (draw scn)
  (place-image (square SIZE "solid" "red")
               (send this x-px)
               (send this y-px)
               scn))
}}
 

And now @racket[move]:

@filebox[@r[seg%]]{
@classblock{
(check-expect (send (new seg% 0 0) move "up")    (new seg%  0  1))
(check-expect (send (new seg% 0 0) move "down")  (new seg%  0 -1))
(check-expect (send (new seg% 0 0) move "left")  (new seg% -1  0))
(check-expect (send (new seg% 0 0) move "right") (new seg%  1  0))
(define (move d)
  (cond [(string=? d "up")    
         (new seg% (send this x) (add1 (send this y)))]
        [(string=? d "down")  
         (new seg% (send this x) (sub1 (send this y)))]
        [(string=? d "left")  
         (new seg% (sub1 (send this x)) (send this y))]
        [(string=? d "right") 
         (new seg% (add1 (send this x)) (send this y))]))
}}

And now @racket[on-board?]:

@filebox[@r[seg%]]{
@classblock{
(check-expect (send (new seg% 0  0) on-board?) true)
(check-expect (send (new seg% 0 -1) on-board?) false)
(check-expect (send (new seg% 0 (sub1 HEIGHT)) on-board?) true)
(check-expect (send (new seg% 0 HEIGHT) on-board?) false)
(define (on-board?)
  (and (<= 0 (send this x) (sub1 WIDTH))
       (<= 0 (send this y) (sub1 HEIGHT))))
}}

And finally, the @racket[x-px] and @racket[y-px] methods:

@filebox[@r[seg%]]{
@classblock{
(check-expect (send (new seg% 0 0) x-px) (* 1/2 SIZE))
(check-expect (send (new seg% 0 0) y-px) (- HEIGHT-PX (* 1/2 SIZE)))
(define (x-px)
  (* (+ 1/2 (send this x)) SIZE))
(define (y-px)
  (- HEIGHT-PX (* (+ 1/2 (send this y)) SIZE)))
}}

That completes all of the obligations of the @racket[seg%] interface.

@section{Another implementation of coordinates: food}

Food is another implementation of the @racket[coord<%>] interface, and
it is largely similar to the @racket[seg%] class, which suggests that
@racket[seg%] and @racket[food%] may be good candidates for
abstraction, but that's something to worry about later.  For now,
let's implement @racket[food%].  Since we've already been through the
design of @racket[seg%], we'll do @racket[food%] quickly:


@classblock{
;; A Food is a (new food% Nat Nat) is a Coord
(define-class food%
  (fields x y)

  (define (same-pos? c)
    (and (= (send this x) (send c x))
         (= (send this y) (send c y))))    

  (define (draw scn)
    (place-image (square SIZE "solid" "green")
                 (send this x-px)
                 (send this y-px)
                 scn))

  (define (move d)
    (cond [(string=? d "up")    
           (new food% (send this x) (add1 (send this y)))]
          [(string=? d "down")  
           (new food% (send this x) (sub1 (send this y)))]
          [(string=? d "left")  
           (new food% (sub1 (send this x)) (send this y))]
          [(string=? d "right") 
           (new food% (add1 (send this x)) (send this y))]))

  (define (on-board?)
    (and (<= 0 (send this x) (sub1 WIDTH))
         (<= 0 (send this y) (sub1 HEIGHT))))

  (define (x-px)
    (* (+ 1/2 (send this x)) SIZE))
  (define (y-px)
    (- HEIGHT-PX (* (+ 1/2 (send this y)) SIZE))))
}

You'll notice that this class definition is nearly identical to the
definition of @racket[seg%].  The key differences are in @racket[move]
and @racket[draw].  We'll hold off on abstracting for now.

@section{Representing the snake}


What information needs to be represented in a snake?

@itemlist[
  @item{Direction}
  @item{Segments}
]

What are the operations we need to perform on snakes?

@classblock{
;; A Snake implements:

;; move : -> Snake
;; Move this snake in its current direction.

;; grow : -> Snake
;; Grow this snake in its current direction.

;; turn : Dir -> Snake
;; Turn this snake in the given direction.

;; draw : Scene -> Scene
;; Draw this snake on the scene.
}

Here's a possible data definition:

@classblock{
;; A (new snake% Dir [Listof Seg]) is a Snake
(define-class snake%
  (fields dir segs))
}

But after a moment of reflection, you will notice that a snake with no
segments doesn't make sense---a snake should always have at least one
segment.  Moreover, we need to settle on an interpretion of the order
of the list; either the front of the list is interpreted as the front
of the snake or the rear of the list is interpreted as the front of
the snake.  Together, non-emptiness and order let us determine which
element is the head of the snake.

Here's our revised data definition:

@classblock{
;; A (new snake% Dir (cons Seg [Listof Seg])) is a Snake
(define-class snake%
  (fields dir segs)
  ...)
}

An alternative data definition that might be worth considering is:

@classblock{
;; A Snake is a (new snake% Dir Seg [Listof Seg])
(define-class snake%
  (fields dir head segs))
}

But for the time being let's stick with the former one.

Now let's implement the interface.  Here's the template:

@classblock{
;; ? ... -> ?
(define (snake-template ...)
  (send this dir) ... (send this segs) ...)
}

The @racket[move] method works by moving the head of the snake and 
dropping the last element of the list of segments:

@classblock{
(check-expect (send (new snake% "right" (list (new seg% 0 0))) move)
              (new snake% "right" (list (new seg% 1 0))))
}

@filebox[@r[snake%]]{
@classblock{
(define (move)
  (new snake%
       (send this dir)
       (cons (send (first (send this segs)) move (send this dir))
             (all-but-last (send this segs)))))
}}

This relies on a helper function, @racket[all-but-last], which is
straightforward to write (recall that @racket[segs] is a non-empty
list):

@classblock{
(check-expect (all-but-last (list "x")) empty)
(check-expect (all-but-last (list "y" "x")) (list "y"))

;; (cons X [Listof X]) -> [Listof X]
;; Drop the last element of the given list.
(define (all-but-last ls)
  (cond [(empty? (rest ls)) empty]
        [else (cons (first ls)
                    (all-but-last (rest ls)))]))
}


The @racket[grow] method is much like @racket[move], except that no
element is dropped from the segments list:

@filebox[@r[snake%]]{
@classblock{
(check-expect (send (new snake% "right" (list (new seg% 0 0))) grow)
              (new snake% "right" (list (new seg% 1 0) 
                                        (new seg% 0 0))))
(define (grow)
  (new snake% 
       (send this dir)
       (cons (send (first (send this segs)) move (send this dir))
             (send this segs))))
}}

Now let's write the @racket[turn] method:

@filebox[@r[snake%]]{
@classblock{
(check-expect (send (new snake% "left" (list (new seg% 0 0))) turn "up")
              (new snake% "up" (list (new seg% 0 0))))
(define (turn d)
  (new snake% d (send this segs)))
}}

And finally, @racket[draw]:

@filebox[@r[snake%]]{
@classblock{
(check-expect (send (new snake% "left" (list (new seg% 0 0))) draw MT-SCENE)
              (send (new seg% 0 0) draw MT-SCENE))
(define (draw scn)
  (foldl (λ (s scn) (send s draw scn))
         scn
         (send this segs)))
}}

As this method shows, functions and methods can co-exist nicely in a
single language.

@section{Seeing the world}

At this point we have a working but incomplete system and we can
interact with it in the interactions window:

<CODE REMOVED DUE TO SEG FAULT>

@void{
(the-eval
'(begin
  (require 2htdp/image)
  (require class/universe)

  (define WIDTH   8) ; in grid units
  (define HEIGHT  8) ; in grid units
  (define SIZE   32) ; in pixels / grid unit
  (define WIDTH-PX  (* SIZE WIDTH))  ; in pixels
  (define HEIGHT-PX (* SIZE HEIGHT)) ; in pixels
  (define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))

  ;; A World is a (new world% Snake Food).
  (define-class world%
    (fields snake food)

    (define (on-tick)
      (new world% 
           (send (send this snake) move)
           (send this food)))

    (define (tick-rate) 1/8)

    (define (to-draw)
      (send (send this food) draw
            (send (send this snake) draw MT-SCENE))))

  ;; A Coord implements:
  
  ;; same-pos? : Coord -> Boolean
  ;; Is this coordinate at the same position as the given one?
  
  ;; draw : Scene -> Scene
  ;; Draw this coordinate on the scene.
  
  ;; move : Dir -> Coord
  ;; Move this coordinate in the given direction.
  
  ;; on-board? : -> Boolean
  ;; Is this coordinate on the board?
  
  ;; {x,y} : -> Nat
  ;; The {x,y}-component of grid-coordinate.
  
  ;; {x-px,y-px} : -> Nat
  ;; The {x,y}-component of pixel-graphics-coordinate.
  
  ;; A Dir is one of:
  ;; - "left"
  ;; - "right"
  ;; - "up"
  ;; - "down"

  ;; A (new seg% Int Int) is a Coord
  ;; Interp: represents a segment grid-coordinate.
  (define-class seg%
    (fields x y)
    (define (same-pos? c)
      (and (= (send this x) (send c x))
           (= (send this y) (send c y))))
    (define (draw scn)
      (place-image (square SIZE "solid" "red")
                   (send this x-px)
                   (send this y-px)
                   scn))
    (define (move d)
      (cond [(string=? d "up")    
             (new seg% (send this x) (add1 (send this y)))]
            [(string=? d "down")  
             (new seg% (send this x) (sub1 (send this y)))]
            [(string=? d "left")  
             (new seg% (sub1 (send this x)) (send this y))]
            [(string=? d "right") 
             (new seg% (add1 (send this x)) (send this y))]))
    (define (on-board?)
      (and (<= 0 (send this x) (sub1 WIDTH))
           (<= 0 (send this y) (sub1 HEIGHT))))
    (define (x-px)
      (* (+ 1/2 (send this x)) SIZE))
    (define (y-px)
      (- HEIGHT-PX (* (+ 1/2 (send this y)) SIZE))))

  (check-expect (send (new seg% 0 0) same-pos? (new seg% 0 0)) true)
  (check-expect (send (new seg% 0 0) same-pos? (new seg% 1 0)) false)
  (check-expect (send (new seg% 0 0) draw MT-SCENE)
                (place-image (square SIZE "solid" "red")
                             (* 1/2 SIZE)
                             (- HEIGHT-PX (* 1/2 SIZE))
                             MT-SCENE))
  (check-expect (send (new seg% 0 0) move "up")    (new seg%  0  1))
  (check-expect (send (new seg% 0 0) move "down")  (new seg%  0 -1))
  (check-expect (send (new seg% 0 0) move "left")  (new seg% -1  0))
  (check-expect (send (new seg% 0 0) move "right") (new seg%  1  0))
  (check-expect (send (new seg% 0  0) on-board?) true)
  (check-expect (send (new seg% 0 -1) on-board?) false)
  (check-expect (send (new seg% 0 (sub1 HEIGHT)) on-board?) true)
  (check-expect (send (new seg% 0 HEIGHT) on-board?) false)
  (check-expect (send (new seg% 0 0) x-px) (* 1/2 SIZE))
  (check-expect (send (new seg% 0 0) y-px) (- HEIGHT-PX (* 1/2 SIZE)))
  
  ;; A Food is a (new food% Nat Nat) is a Coord
  (define-class food%
    (fields x y)

    (define (same-pos? c)
      (and (= (send this x) (send c x))
           (= (send this y) (send c y))))    

    (define (draw scn)
      (place-image (square SIZE "solid" "green")
                   (send this x-px)
                   (send this y-px)
                   scn))

    (define (move d)
      (cond [(string=? d "up")    
             (new food% (send this x) (add1 (send this y)))]
            [(string=? d "down")  
             (new food% (send this x) (sub1 (send this y)))]
            [(string=? d "left")  
             (new food% (sub1 (send this x)) (send this y))]
            [(string=? d "right") 
             (new food% (add1 (send this x)) (send this y))]))

    (define (on-board?)
      (and (<= 0 (send this x) (sub1 WIDTH))
           (<= 0 (send this y) (sub1 HEIGHT))))

    (define (x-px)
      (* (+ 1/2 (send this x)) SIZE))
    (define (y-px)
      (- HEIGHT-PX (* (+ 1/2 (send this y)) SIZE))))

  ;; A Snake implements:
  
  ;; move : -> Snake
  ;; Move this snake in its current direction.
  
  ;; grow : -> Snake
  ;; Grow this snake in its current direction.
  
  ;; turn : Dir -> Snake
  ;; Turn this snake in the given direction.
  
  ;; draw : Scene -> Scene
  ;; Draw this snake on the scene.
  
  ;; A (new snake% Dir Seg [Listof Seg]) is a Snake
  (define-class snake%
    (fields dir segs)
    (define (move)
      (new snake%
           (send this dir)
           (cons (send (first (send this segs)) move (send this dir))
                 (all-but-last (send this segs)))))

    (define (grow)
      (new snake%
           (send this dir)
           (cons (send (first (send this segs)) move (send this dir))
                 (send this segs))))
    
    (define (turn d)
      (new snake% d (send this segs)))

    (define (draw scn)
      (foldl (λ (s scn) (send s draw scn))
             scn
             (send this segs))))

  (check-expect (send (new snake% "right" (list (new seg% 0 0))) move)
                (new snake% "right" (list (new seg% 1 0))))
  (check-expect (send (new snake% "right" (list (new seg% 0 0))) grow)
                (new snake% "right" (list (new seg% 1 0)
                                          (new seg% 0 0))))
  (check-expect (send (new snake% "left" (list (new seg% 0 0))) turn "up")
                (new snake% "up" (list (new seg% 0 0))))
  (check-expect (send (new snake% "left" (list (new seg% 0 0))) draw MT-SCENE)
                (send (new seg% 0 0) draw MT-SCENE))

  (check-expect (all-but-last (list "x")) empty)
  (check-expect (all-but-last (list "y" "x")) (list "y"))

  ;; (cons X [Listof X]) -> [Listof X]
  ;; Drop the last element of the given list.
  (define (all-but-last ls)
    (cond [(empty? (rest ls)) empty]
          [else (cons (first ls)
                      (all-but-last (rest ls)))]))
))}

@examples[#:eval the-eval
  (define w0 (new world% 
                  (new snake% 
                       "right" 
                       (list (new seg% 5 1)
                             (new seg% 5 0)
                             (new seg% 4 0)))
                  (new food% 3 4)))
   (send w0 to-draw)
   (send (send w0 on-tick) to-draw)
   (send (send (send w0 on-tick) on-tick) to-draw)]

We'll leave it at this point and further the refine the program in the
future.

@section{The whole ball of wax}

@codeblock{
#lang class/0
(require 2htdp/image)
(require class/universe)

(define WIDTH   8) ; in grid units
(define HEIGHT  8) ; in grid units
(define SIZE   32) ; in pixels / grid unit
(define WIDTH-PX  (* SIZE WIDTH))  ; in pixels
(define HEIGHT-PX (* SIZE HEIGHT)) ; in pixels
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))

;; A World is a (new world% Snake Food).
(define-class world%
  (fields snake food)

  (define (on-tick)
    (new world% 
         (send (send this snake) move)
         (send this food)))

  (define (tick-rate) 1/8)

  (define (to-draw)
    (send (send this food) draw
          (send (send this snake) draw MT-SCENE))))

;; A Coord implements:

;; same-pos? : Coord -> Boolean
;; Is this coordinate at the same position as the given one?

;; draw : Scene -> Scene
;; Draw this coordinate on the scene.

;; move : Dir -> Coord
;; Move this coordinate in the given direction.

;; on-board? : -> Boolean
;; Is this coordinate on the board?

;; {x,y} : -> Nat
;; The {x,y}-component of grid-coordinate.

;; {x-px,y-px} : -> Nat
;; The {x,y}-component of pixel-graphics-coordinate.

;; A Dir is one of:
;; - "left"
;; - "right"
;; - "up"
;; - "down"

;; A (new seg% Int Int) is a Coord
;; Interp: represents a segment grid-coordinate.
(define-class seg%
  (fields x y)
  (check-expect (send origin same-pos? (new seg% 0 0)) true)
  (check-expect (send origin same-pos? (new seg% 1 0)) false)
  (define (same-pos? c)
    (and (= (send this x) (send c x))
         (= (send this y) (send c y))))
  (define (draw scn)
    (place-image (square SIZE "solid" "red")
                 (send this x-px)
                 (send this y-px)
                 scn))
  (define (move d)
    (cond [(string=? d "up")    
           (new seg% (send this x) (add1 (send this y)))]
          [(string=? d "down")  
           (new seg% (send this x) (sub1 (send this y)))]
          [(string=? d "left")  
           (new seg% (sub1 (send this x)) (send this y))]
          [(string=? d "right") 
           (new seg% (add1 (send this x)) (send this y))]))
  (define (on-board?)
    (and (<= 0 (send this x) (sub1 WIDTH))
         (<= 0 (send this y) (sub1 HEIGHT))))
  (define (x-px)
    (* (+ 1/2 (send this x)) SIZE))
  (define (y-px)
    (- HEIGHT-PX (* (+ 1/2 (send this y)) SIZE))))

(check-expect (send (new seg% 0 0) draw MT-SCENE)
              (place-image (square SIZE "solid" "red")
                           (* 1/2 SIZE)
                           (- HEIGHT-PX (* 1/2 SIZE))
                           MT-SCENE))
(check-expect (send (new seg% 0 0) move "up")    (new seg%  0  1))
(check-expect (send (new seg% 0 0) move "down")  (new seg%  0 -1))
(check-expect (send (new seg% 0 0) move "left")  (new seg% -1  0))
(check-expect (send (new seg% 0 0) move "right") (new seg%  1  0))
(check-expect (send (new seg% 0  0) on-board?) true)
(check-expect (send (new seg% 0 -1) on-board?) false)
(check-expect (send (new seg% 0 (sub1 HEIGHT)) on-board?) true)
(check-expect (send (new seg% 0 HEIGHT) on-board?) false)
(check-expect (send (new seg% 0 0) x-px) (* 1/2 SIZE))
(check-expect (send (new seg% 0 0) y-px) (- HEIGHT-PX (* 1/2 SIZE)))

;; A Food is a (new food% Nat Nat) is a Coord
(define-class food%
  (fields x y)

  (define (same-pos? c)
    (and (= (send this x) (send c x))
         (= (send this y) (send c y))))    

  (define (draw scn)
    (place-image (square SIZE "solid" "green")
                 (send this x-px)
                 (send this y-px)
                 scn))

  (define (move d)
    (cond [(string=? d "up")    
           (new food% (send this x) (add1 (send this y)))]
          [(string=? d "down")  
           (new food% (send this x) (sub1 (send this y)))]
          [(string=? d "left")  
           (new food% (sub1 (send this x)) (send this y))]
          [(string=? d "right") 
           (new food% (add1 (send this x)) (send this y))]))

  (define (on-board?)
    (and (<= 0 (send this x) (sub1 WIDTH))
         (<= 0 (send this y) (sub1 HEIGHT))))

  (define (x-px)
    (* (+ 1/2 (send this x)) SIZE))
  (define (y-px)
    (- HEIGHT-PX (* (+ 1/2 (send this y)) SIZE))))

;; A Snake implements:

;; move : -> Snake
;; Move this snake in its current direction.

;; grow : -> Snake
;; Grow this snake in its current direction.

;; turn : Dir -> Snake
;; Turn this snake in the given direction.

;; draw : Scene -> Scene
;; Draw this snake on the scene.

;; A (new snake% Dir Seg [Listof Seg]) is a Snake
(define-class snake%
  (fields dir segs)
  (define (move)
    (new snake%
         (send this dir)
         (cons (send (first (send this segs)) move (send this dir))
               (all-but-last (send this segs)))))

  (define (grow)
    (new snake%
         (send this dir)
         (cons (send (first (send this segs)) move (send this dir))
               (send this segs))))
  
  (define (turn d)
    (new snake% d (send this segs)))

  (define (draw scn)
    (foldl (λ (s scn) (send s draw scn))
           scn
           (send this segs))))

(define origin (new seg% 0 0))
(check-expect (send (new snake% "right" (list (new seg% 0 0))) move)
              (new snake% "right" (list (new seg% 1 0))))
(check-expect (send (new snake% "right" (list (new seg% 0 0))) grow)
              (new snake% "right" (list (new seg% 1 0)
                                        (new seg% 0 0))))
(check-expect (send (new snake% "left" (list (new seg% 0 0))) turn "up")
              (new snake% "up" (list (new seg% 0 0))))
(check-expect (send (new snake% "left" (list (new seg% 0 0))) draw MT-SCENE)
              (send (new seg% 0 0) draw MT-SCENE))

(check-expect (all-but-last (list "x")) empty)
(check-expect (all-but-last (list "y" "x")) (list "y"))

;; (cons X [Listof X]) -> [Listof X]
;; Drop the last element of the given list.
(define (all-but-last ls)
  (cond [(empty? (rest ls)) empty]
        [else (cons (first ls)
                    (all-but-last (rest ls)))]))

(big-bang (new world% 
               (new snake% 
                    "right" 
                    (list (new seg% 5 1)
                          (new seg% 5 0)
                          (new seg% 4 0)))
              (new food% 3 4)))
}

@include-section{05/exercises.scrbl}
