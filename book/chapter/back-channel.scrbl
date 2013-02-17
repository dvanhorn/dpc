#lang scribble/manual
@(require class/utils
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/1 check-expect define-struct ... length
                                numerator denominator))
          (for-label 2htdp/image)
          (for-label (only-in test-engine/racket-tests check-expect))
          (for-label class/universe))

@title{Back-channels}

Up until this point, computations communicate by consuming arguments
and producing values.  Mutation enables more channels of communication
between computations via @emph{shared} mutable data.

As an example, recall our counter world program from @secref{counter}:

@classblock{
;; A Counter is a (counter-world% Natural)
(define-class counter-world%
  (fields n)
  ...
  ;; on-tick : -> Counter
  (define (on-tick)
    (new counter-world% (add1 (this . n)))))

(big-bang (new counter-world% 0))
}

To illustrate how mutable data provides alternative channels of
communication, let's develop a variant of the program that
communicates the state of the world through an implicit stateful
back-channel.

@classblock{
;; A Counter is a (counter-world% Natural)
(define-class counter-world%
   (fields n)
   ...
   ;; on-tick : -> Counter
   (define (on-tick)
     (begin (set-field! n (add1 (send this n)))
            this)))

(big-bang (new counter-world% 0))
}

Notice how the new @r[on-tick] method doesn't produce a new
@r[counter-world%] object with an incremented counter; instead it
@emph{mutates} its own counter and returns its (mutilated) self.

You'll find that this program appears to behave just like the old
version, but backchannels open up new forms of interaction (and
interference) that may not be intended.  For example, can you predict
what thise program will do?

@classblock{
(launch-many-worlds (big-bang (new counter-world% 0))
                    (big-bang (new counter-world% 0)))
}

How about this (seemingly equivalent) one?

@classblock{
(define w (new counter-world% 0))
(launch-many-worlds (big-bang w)
                    (big-bang w))
}
