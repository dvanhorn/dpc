#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt"
          (for-label class5/universe
                     2htdp/image))

@(define exercise (exercise-counter))

@title[#:tag "lab12"]{3/28: Mixins, sources, sinks, and projectiles}

@exercise{
  Start with this code: @url["source-sink.rkt"]. It creates a @tt{World} with a
  @tt{Source} and a @tt{Sink} (the circles), along with a few @tt{Projectiles}
  (the triangles).

  Read over the code and understand how it works. Note the use of the
  @racket[point-mixin] to abstract the common behavior involving location,
  velocity, and acceleration.
}

@exercise{
  Add a @racket[contains?] @tt{: Location -> Boolean} method to @tt{Source} that
  tests whether the given point is within the circle represented by the
  @tt{Source}.
}

@exercise{
  We will want to add and remove @tt{Projectile}s over time. Add two methods,
  @racket[add-projectile!] @tt{: Projectile -> Void} and
  @racket[remove-projectile!] @tt{: Projectile -> Void} to @tt{World}. Use
  intensional equality (@racket[eq?]) to decide which @tt{Projectile} to remove.
}

@exercise{
  Add a @racket[on-mouse] method to @tt{World}. When the user clicks on the
  @tt{Source} object, add a new @tt{Projectile} to the @tt{World} at the
  @tt{Source}'s location, with a random (fast) velocity.
}

@exercise{
  Add the @racket[contains?] to @tt{Sink}s---abstract the common behavior out
  into a @racket[circle-mixin].
}

@exercise{
  Modify @tt{World}'s @racket[on-tick] method so that @tt{Projectile}s that
  collide with the @tt{Sink} are removed from the @tt{World}.

  Since @tt{Projectile}s are triangles, we won't do real collision detection
  here---just test whether the @tt{Projectile}'s location is within the
  @tt{Sink}'s circle.
}

@exercise{
  As it stands @tt{Projectile}s wander around rather aimlessly. But note that
  we've given both @tt{Projectile}s and the @tt{Sink} a @racket[mass] field...

  Abstract these two @racket[mass] fields into a @racket[mass-mixin]. Define
  something to implement @tt{Massful} when it has a @racket[mass] (field or
  method).

  Add an @racket[add-mass!] @tt{: Mass -> Void} method to @racket[mass-mixin]
  that increments our @racket[mass] by the given amount.

  Add a @racket[gravity] @tt{: Massful -> Acceleration} method to
  @racket[mass-mixin] that computes the acceleration that one @tt{Massful}
  object induces on another.

  Finally, in @tt{World}'s @racket[on-tick] method, update the acceleration for
  each @tt{Projectile} so that they gravitate toward the @tt{Sink}.
}

@exercise{
  Let's loosen up a little: let's make the @tt{Sink} pulse its color over time,
  the @tt{Source} pulse its size over time, and the @tt{Projectile}s rotate over
  time. This won't actually be very difficult.

  Add a @racket[time-mixin] to all three classes. The mixin should add a
  @racket[time] field and hook into the @racket[tick!] method to increment
  @racket[time] by the @racket[TICK-RATE]. (You'll have to modify all your
  @racket[random-]@emph{foo} methods to initialize @racket[time] to something,
  like @racket[0].)

  Then modify the appropriate @racket[color], @racket[radius], or @racket[draw]
  methods to modulate their output based on the @racket[time].
}
