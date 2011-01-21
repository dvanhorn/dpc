#lang scribble/manual
@(require "../utils.rkt"
          (for-label class1))

@title[#:tag "assign03"]{1/26: Zombie, redux}

Due: 1/26.

@itemlist[#:style 'ordered 
 @item{@bold{Zombie!}

 Language: @racketmodname[class0].

 Revise your previous design of the Zombie! game.  Make sure to fix
 any problems that remained when you submitted last week.  Eliminate
 any duplicated code by using the functional abstraction design
 recipe.

 This part of the assignment must be completed in @racketmodname[class0],
 so you cannot use inheritance.

 If you are totally satisfied with your previous submission, you may
 simply submit your previous version of Zombie for this portion of the
 assignment.

 You will likely not have grader feedback in time to incorporate
 tutors' comments into your re-design, so you should revise your
 program as you see fit.  This portion of the assignment @emph{must}
 be based on your previous solution.  If you throw out all of your
 code and submit the solution provided on the web page, you will
 receive no credit for this portion of the assignment.}

 @item{@bold{Super Zombie!}

 Language: @racketmodname[class1]. 

 Revise your design of the Zombie game to include a @racket[zombie<%>]
 and @racket[player<%>] interface.  Implement a @racket[live-zombie%]
 and @racket[dead-zombie%] class that both implement your
 @racket[zombie<%>] interface; implement a @racket[player%] class that
 implements your @racket[player<%>] interface.

 Do not use the functional abstraction recipe.  Instead, if you notice
 code that code can be shared between the various classes you've
 designed, design super classes and use inheritance to abstract the
 duplicated code.

 Design a @racket[world%] class for playing the Zombie game that
 interacts with the zombie and player objects only according to the
 interfaces you've designed, i.e. the @racket[world%] class should
 work for @emph{any} objects that correctly implement your zombie and 
 player interfaces.}

 @item{@bold{Modulo Zombie!}

 Language: @racketmodname[class1]. 

 Using your interface design from the previous problem, design a 
 @racket[modulo-player%] class and a @racket[modulo-live-zombie%]
 class that implement the @racket[player<%>] and @racket[zombie<%>]
 interfaces, respectively.

 These alternative implementations should behave as follows: the
 player and the zombies may now "wrap around" on the screen.  If a
 player goes off the top of the screen, they should re-appear on the
 bottom; if the go off of the left side, they should appear on the
 right, etc., and likewise for the zombies.  When calculating in which
 direction they should go, the player and zombies should take into
 account the possibility of wrapping around the screen.  So for
 example, if the player is on the far right side and there is a zombie
 on the far left side, the zombie should head left to wrap around the
 screen and quickly arrive upon the player and feast upon his or her
 brains.  Similarly if the mouse is on the very top and the player is
 on the very bottom, the player should move down to get to the top
 quickly.

 If you need to make changes to your interface design to accomodate
 these new game requirements, you must re-implement your solution to
 problem 2 in order to satisfy the revised interfaces.  In the end,
 the interfaces used and your implementation of the @racket[world%]
 class be the same in both problem 2 and 3.}

 @item{@bold{Mixed Zombie!}

 Language: @racketmodname[class1]. 

 Experiment with different combinations of your classes from part 2
 and 3 (only the player can wrap around; only the zombies can wrap
 around; some of the zombies and the player; some of the zombies, but
 not the player, etc.) until you find a combination you like best.
 Write down an expression that launches the game using this
 combination.}

 @item{@bold{Finger exercises}

 Language: @racketmodname[class1]. 

 Consider the parametric data definition for lists we studied last
 semester:

 @#reader scribble/comment-reader
(racketblock 
 ;; A [Listof X] is one of:
 ;; - empty
 ;; - (cons X [Listof X])
)

Design an analogous class-based representation of parametric lists.
Design a @racket[list<%>] interface that includes @racket[length],
@racket[append], @racket[reverse], @racket[map], @racket[filter],
@racket[foldl], and @racket[foldr].

Implement that interface in two ways:
@itemlist[

 @item{Using the recipe for a recursive union represented using
objects, i.e.  similar to the way you developed lists of numbers last
week.}

 @item{Using a "wrapper class", i.e. design a class that has a single
field which contains a "real" list---one built out of @racket[cons]
and @racket[empty].}

]

Any program that interacts with either of these representations
according to the interface should not be able to tell them apart.

Use inheritance to lift method definitions to a super class to the
full extent possible.  (Hint: it will help if you realize that many of
these methods may be expressed in terms of a few "core" methods.)  If
possible, have both the recursive union representation and the wrapper
representation share a common super class.
 }
]





