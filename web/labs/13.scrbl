#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt"
          (for-label class5/universe
                     2htdp/image))

@(define exercise (exercise-counter))

@title[#:tag "lab13"]{4/04: The minority game}

Here is a state-transition diagram for a simple @math{n}-player game:

  @elem[@image["labs/13/state-diagram.png"] #:style "center"]

@itemlist[
  @item{In the ``Join'' state players gather. At some arbitrary point it is
  decided to start the game with the current set of players.}
  @item{In the ``Vote'' state each player chooses to vote either A or B. After
  everyone has placed a vote, play advances.}
  @item{In the ``Win/Lose'' state everyone is notified whether they've won or
  lost the game. After everyone sees their result, play restarts in the ``Join''
  state, where players can come and go before another round starts.}
]

The rules are very simple: players in the minority win. So if three people vote
A and two vote B, then the three lose and the two win. If there are six players,
three of which vote A and three of which vote B, then everyone loses because
there is no minority. If there's only one player then they can never win because
they will never be in the (empty) minority. Also, with two players no one ever
wins. So it's really only interesting in groups of three or more.

This game sounds pretty dull but, like many other seemingly dull games, it's
useful in the field of @emph{game theory} to analyze complex behaviors among
large groups of interacting agents, be they people in social situations,
entities in financial markets, or even matter in physical systems. This
particular game is called a ``minority game'' and you can read
@link["http://news.softpedia.com/news/Minority-Games-38625.shtml"]{more}
@link["http://en.wikipedia.org/wiki/Minority_game#Minority_Game"]{about}
@link["http://www.google.com/search?q=minority+game"]{it} if you're curious.

Our interest in this game isn't its game-theoretic properties, but how we can
code it up using @tt{World}s and @tt{Universe}s. In particular, we'll see a way
to model state-transition systems by mapping each state down to its own class,
with objects transitioning from one state to the next by morphing into new kinds
of objects.

@exercise{
  The @tt{Universe} will be responsible for coordinating the players, gathering
  their votes, and determining which players win and lose.

  Create three @tt{Universe} classes, @racket[join-universe%],
  @racket[vote-universe%], and @racket[win-lose-universe%], one for each state
  in the transition system.

  @itemlist[
    @item{@racket[join-universe%]: Listen for new players for a moment, and then
    send a message to all known players to announce that voting will begin. The
    announcement doesn't need to contain any meaningful information. Incoming
    messages can be ignored.}

    @item{@racket[vote-universe%]: Wait for all players to place a vote
    (@racket["A"] or @racket["B"]). After all votes are in, notify each player
    whether they've won (@racket[true]) or lost (@racket[false]) and advance to
    the next state. If a player places multiple votes, ignore all but the last
    one. Players that try to join during the voting phase can be ignored.}

    @item{@racket[win-lose-universe%]: Give the players a moment to accept their
    defeat or celebrate their victory, then start the game over in the Join
    state and notify the players of this. Again, this message doesn't have to
    contain meaningful information. New players and incoming messages can be
    ignored in this state.} ]

  A few limitations to mind:
  @itemlist[
    @item{Even though we have multiple @tt{Universe}s, there can only be one
    @racket[tick-rate] and, if you want to specify it, it must be defined on the
    initial @tt{Universe}.}

    @item{Each @tt{Universe} can have its own methods like @racket[on-tick],
    @racket[on-disconnect], etc., but if they aren't defined on the initial
    @tt{Universe} then the system won't know to look for it on subsequent
    @tt{Universe}s. If necessary, you can define ``do-nothing'' methods on your
    initial @tt{Universe} that leave everything unchanged.}
  ]
}

@exercise{
  The @tt{World} will represent a player and is reponsible for connecting to the
  universe, picking a vote, and reporting the outcome.

  Create three @tt{World} classes, @racket[join-world%], @racket[vote-world%],
  and @racket[win-lose-world%], one for each state in the transition system.

  @itemlist[
    @item{@racket[join-world%]: Register with the @tt{Universe} and wait to be
    notified that voting has begun. As above, the notification won't contain any
    meaningful information.}

    @item{@racket[vote-world%]: Let the player choose their vote and send it to
    the @tt{Universe} (@racket["A"] or @racket["B"]). Listen for the message
    indicating whether we've won (@racket[true]) or lost (@racket[false]), and
    then advance to the next state.}

    @item{@racket[win-lose-world%]: Report the player's fate and wait to be
    notified that the game is restarting. As above, the incoming message won't
    contain meaningful information.}
  ]

  @tt{World}s have the same limitations as @tt{Universe}s described above.
}

Does your game work properly? Try playing with your neighbors---if you followed
the spec above, you should all be speaking the same protocol.

@exercise{
  I bet you have some repeated code. Since each @tt{Universe} must have
  @racket[on-new] and @racket[on-msg] methods, There should be a few places in
  your three @tt{Universe} classes where you wanted to ignore new connections or
  incoming messages, and you had to define do-nothing methods to appease the
  @racket[universe] system.

  Define @racket[ignore-new-mixin] @tt{: Class -> Class} that adds a do-nothing
  @racket[on-new] method to a class to make it more @tt{Universe} compliant.

  Define @racket[ignore-msg-mixin] @tt{: Class -> Class} that adds a do-nothing
  @racket[on-msg] method to a class to make it more @tt{Universe} compliant.

  Use these two mixins to clean up your @tt{Universe} classes.
}

@exercise{
  If you used @racket[on-key] or @racket[on-mouse] to input the player's vote,
  you'll have a similar problem where you defined do-nothing methods on all but
  one of your @tt{World} classes.

  Clean up your @tt{World} classes by defining a mixin, similar to the two
  above.
}

@exercise{
  If a player tries to join during the Vote or Win/Lose state they will be
  ignored---that's pretty rude!

  Change your @racket[vote-universe%] and @racket[win-lose-universe%] so that
  they queue up incoming players and pass them off to the
  @racket[join-universe%] when the next round starts. To avoid repeated code,
  define and use a @racket[queueing-mixin] to achieve this.
}
