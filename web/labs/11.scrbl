#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt")

@(define exercise (exercise-counter))

@title[#:tag "lab11"]{3/21: Blobs, eating}

@exercise{
  Install @url["class4-blobs.plt"] to get the the @racket[class4/blobs] module.
}

@lab:section{Blobs}

@#reader scribble/comment-reader
(racketblock
; A  World        is a (base-world% Player [Listof Enemy])
; A  Player       is a Blob
; An Enemy        is a Blob
; A  Blob         is a (base-blob% Location Velocity Acceleration Area Color)
; A  Location     is a Complex
; A  Velocity     is a Complex
; An Acceleration is a Complex
; An Area         is a positive Real
; A  Color        is a String
)

@exercise{
  Construct an example @tt{World} with a @tt{Player} and a few @tt{Enemy}s and
  @racket[big-bang] it.
}

@exercise{
  Constructing @tt{Worlds} by hand is tedious and a pain. To make things more
  interesting, write a function @racket[random-world] @tt{: -> World} that
  randomly generates a @tt{World} for you.

  To do this, you will probably want to define functions like
  @racket[random-player], @racket[random-enemy], @racket[random-location],
  @racket[random-velocity], and @racket[random-area]---experiment with various
  parameters for these. The scene is 500×500, so the top left is @racket[0+0i]
  and the bottom right is @racket[500+500i]. I gently suggest that you set
  accelerations to @racket[0] instead of randomly choosing them. Also,
  @racket[random-color] never returns anything greyscale, so feel free to rely
  on that to distinguish the player from the enemies.

  Here are a few random functions from @racket[class4/blobs] that you might find
  helpful:

  @#reader scribble/comment-reader
  (racketblock
  ; random-between : Integer Integer -> Integer
  ; Randomly choose a number between a and b (inclusive)

  ; choose : [Listof X] -> X
  ; Randomly choose an element from the list

  ; random-color : -> Color
  ; Randomly choose a color from a set of common colors

  ; random-complex-within : Complex Complex -> Complex
  ; Randomly choose a complex within the rectangle bounded by z1 and z2
  )
}

@exercise{
  The classes @racket[base-world%] and @racket[base-blob%] are defined for you,
  but we will of course want to extend them with new behavior. Define your own
  classes @racket[world%] and @racket[blob%] that extend the given classes, and
  use them in your @racket[big-bang] and @racket[random-]@emph{foo} methods
  above. You don't have to put anything in them yet---we'll add methods as we
  go.
}

@lab:section{Eating}

You should now have a @tt{World} with a bunch of @tt{Blobs} bouncing around, one
of which will be your @tt{Player}. Before we add controls for the @tt{Player},
let's setup the conflict.

@exercise{
  Define an @racket[eat!]@tt{: Blob -> Void} method on @tt{Blob}s. After the
  call @racket[(a #,dot eat! b)], @racket[a] should gain all of @racket[b]'s
  area and @racket[b]'s area should be @racket[0].

  To do this, define and use a @racket[take!]@tt{: -> Area} method on
  @tt{Blob}s. The call @racket[(b #,dot take!)] should produce @racket[b]'s area
  and leave @racket[b] with @racket[0] area afterward.
}

@exercise{
  One of the things that @racket[base-world%] does for you is @emph{collision
  detection}. When two @tt{Blob}s @racket[a] and @racket[b] overlap, it calls
  either

  @#reader scribble/comment-reader
  (racketblock
    (a #,dot collide! b)
  )

  or

  @#reader scribble/comment-reader
  (racketblock
    (b #,dot collide! a)
  )

  depending on its whim.

  Define a @racket[collide!] method on @tt{Blob}s so that, when two @tt{Blob}s
  collide, the bigger one eats the smaller one. If they're the same size do
  whatever you want, as long as one of them eats the other.
}

Your game should now consist of a helpless @tt{Player} bouncing around among a
horde of @tt{Enemy}s. The big fish eat the small fish, and it's very likely that
you get eaten before you get big.

@exercise{
  Help! Override @racket[on-key] in your @tt{World} to give the @tt{Player} some
  chance of survival. If you @racket[set-field!] the @tt{Player}'s @racket[acc]
  field, physics will take care of the rest.

  While you're at it, add a key that quits the game.
}

Your controls are probably somewhat clumsy... It'd be nice if you could
accelerate left while holding down the @racket["left"] key, and stop
accelerating when you let it go. It'd also be nice if you could accelerate up
and left if you hold down the @racket["up"] and @racket["left"] keys at the same
time.

@exercise{
  If you remove your overriding @racket[on-key] method, then
  @racket[base-world%]'s @racket[on-key] method will do some leg work for you
  and call an @racket[on-key-change]@tt{: [Listof KeyEvent] -> World} method
  anytime any key is pressed or released, passing in the current list of pressed
  keys. It ignores the key repeat rate, only notifying you when a key goes up or
  down.

  Define @racket[on-key-change] on your @tt{World} to make a better control
  scheme for the @tt{Player}.
}

Now you should have decent control over your @tt{Player}. That's nice. But I
still think our game is too unforgiving: when you accidentally brush up against
a big @tt{Enemy}, you're eaten immediately, with no chance of escape.

@exercise{
  Change @racket[eat!]ing so that a @tt{Blob} is eaten slowly over time instead
  of all at once. To do this, add an input to @racket[take!] so that @racket[(b
  #,dot take! A)] only takes @racket[A] area from @racket[b], leaving @racket[b]
  with whatever's left over. (Be careful that you can't take more area than
  @racket[b] actually has!)

  Now change @racket[eat!] so that the bigger @tt{Blob} only eats a little of
  the smaller one. The effect of this will be that it will slowly eat all of it
  over some peroid of time.

  Experiment with the eating rate. The tick rate of the game is @racket[1/20],
  so your @racket[eat!] method will be called 20 times per second. Should bigger
  blobs eat faster? Should small blobs be eaten faster? If two blobs are close
  in size, should they only transfer area very slowly at first, until one
  becomes significantly larger?
}
