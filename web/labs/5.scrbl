#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt"
          (for-label (except-in class2 define-struct)
                     2htdp/image
                     (only-in lang/htdp-intermediate-lambda define-struct)
                     class1/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class2))
    ;(the-eval '(require 2htdp/image))
    ;(the-eval '(require class1/universe))
    the-eval))

@(define exercise (exercise-counter))

@title[#:tag "lab05"]{2/07: Dictionaries}

A @emph{dictionary} is a commonly used data structure that maps @emph{keys} to
@emph{values}. Like an actual dictionary on your bookshelf, the central
operation is to lookup a value given its key, just like you would lookup a
definition given a word. Dictionaries also provide operations to add, remove,
and update their key-value mappings. Here's an example interaction with the
kinds of dictionaries we will build in this lab:

@#reader scribble/comment-reader
(interaction #:eval the-eval
(eval:alts (define d (empty-dict #,dot set 1 "one" #,dot set 2 "two" #,dot set 3 "three"))
           (void))
(eval:alts (d #,dot lookup 1)
           "one")
(eval:alts (d #,dot lookup 2)
           "two")
(eval:alts (d #,dot set 1 "uno" #,dot lookup 1)
           "uno")
(eval:alts (d #,dot set 1 "uno" #,dot lookup 2)
           "two")
(eval:alts (d #,dot has-key? 4)
           false)
(eval:alts (d #,dot has-value? "two")
           true)
)

@lab:section{An interface for dictionaries}

Before we build anything, we should first specify what a dictionary is.

@#reader scribble/comment-reader
(racketblock
; A Key is a Nat

; An [IDict V] implements:
(define-interface dict<%>
  [; Key -> Boolean
   ; Does the given key exist?
   has-key?

   ; Key -> V
   ; The value mapped to by the given key
   ; Assumption: the key exists
   lookup

   ; Key V -> [IDict V]
   ; Set the given key-value mapping
   set])
)

Notice that a dictionary @tt{[IDict V]} is parameterized by the type of its
values, @tt{V}. This enables us to give precise contracts to the methods.

@lab:section{Dictionaries as lists of pairs}

Like most structures we encounter in programming, dictionaries can be
implemented in many different ways. Let's begin with a simple representation: a
list of key-value pairs.

@#reader scribble/comment-reader
(racketblock
; A [ListDict V] is one of:
;  - (ld-empty%)
;  - (ld-cons% Key V [ListDict V])
)

@exercise{
  Define the classes @racket[ld-empty%] and @racket[ld-cons%].
}

@exercise{
  Define the method @racket[has-key?] for @tt{ListDict}s.
}

@exercise{
  Define the method @racket[lookup] for @tt{ListDict}s.
}

@exercise{
  Define the method @racket[set] for @tt{ListDict}s.
}

How good are your tests?---make sure you pass this one:

@#reader scribble/comment-reader
(racketblock
(check-expect ((ld-empty%) #,dot set 1 "one"
                           #,dot set 2 "two"
                           #,dot set 1 "uno"
                           #,dot lookup 1)
              "uno")
)

@exercise{
  Here's an interesting property relating the @racket[set] and @racket[has-key?]
  methods: for all dictionaries @racket[d], keys @racket[k], and values
  @racket[v], if you ask @racket[has-key? k] after setting the mapping
  @racket[k] → @racket[v], then you should always get @racket[true].

  Write this property down as a function that takes an @tt{[IDict V]}, a
  @tt{Key}, a @tt{V}, and returns @racket[true] if the property is satisfied by
  the particular inputs given. (If it ever returns @racket[false], then the
  property is invalid!)
}

@exercise{
  As above, write a property relating @racket[set] and @racket[lookup].
}

To randomly test these properties we need a way to randomly generate
dictionaries.

@exercise{
  Define a function @racket[random-list-dict] that takes a number @racket[n] and
  builds a @tt{[ListDict Key Nat]} of size @racket[n] with random key-value
  mappings. (It's not important that the size is exactly @racket[n].)
}

@exercise{
  Randomly test your properties: for each property, call @racket[check-expect] a
  large number of times, testing the property on random inputs each time.
}

@lab:section{Dictionaries as sorted lists of pairs}

What @tt{ListDict} do you get back if you set the same key twice?

@#reader scribble/comment-reader
(racketblock
((ld-empty%) #,dot set 1 "one" #,dot set 1 "uno")
)

Does your @racket[set] method hunt through the list and remove old mappings for
the key @racket[1], or do you tolerate multiple occurrences of the same key by
ignoring all but the first? Either way works fine, but the question suggests we
use a less ambiguous representation to avoid the issue in the first place.

@#reader scribble/comment-reader
(racketblock
; A [SortedListDict V] is one of:
;  - (sld-empty%)
;  - (sld-cons Key V [SortedListDict V])
;
; Invariant: The keys are sorted (increasing).
)

@exercise{
  Define the classes @racket[sld-empty%] and @racket[sld-cons%].
}

@exercise{
  Define the methods @racket[has-key?], @racket[lookup], and @racket[set] for
  @tt{SortedListDict}s. Pay attention to the invariant!
}

How about randomized testing? The properties we wrote above are generic over any
@tt{IList}s, so we can reuse them to test our @tt{SortedListDict}s.

@exercise{
  Define a function @racket[random-sorted-list-dict], similar to
  @racket[random-list-dict] above.

  Identify and abstract out their common behavior.
}

@exercise{
  Randomly test your @tt{IList} properties on random @tt{SortedListDict}s.
}

The methods you wrote for @tt{SortedListDict} should preserve its sorting
invariant, but nothing prevents client code from combining @racket[sld-empty%]
and @racket[sld-cons%] in ways that violate it. Modules enable us to hide our
implementation details and prevent clients from violating our internal
invariants.

And while we're thinking about module boundaries, let's take a moment to
organize the rest of our code, too.

@exercise{
  Split your code into separate modules:
  @itemlist[
    @item{@racket["dict.rkt"] contains and @racket[provide]s the @tt{IDict}
          interface}
    @item{@racket["list-dict.rkt"] @racket[require]s @racket["dict.rkt"],
          contains the @tt{ListDict} implementation, and @racket[provide]s only
          an empty @tt{ListDict}}
    @item{@racket["sorted-list-dict.rkt"] @racket[require]s @racket["dict.rkt"],
          contains the @tt{SortedListDict} implementation, and @racket[provide]s
          only an empty @tt{SortedListDict}}
    @item{@racket["random-tests.rkt"] @racket[require]s all the other modules,
          and specifies and randomly tests properties for the various kinds of
          dictionaries}
  ]
}

The @tt{IDict} interface stands alone, ready for other modules to implement or
use it. The @tt{ListDict} implementation follows the @tt{IDict} interface and
tests its individual methods. The @tt{SortedListDict} implementation does the
same. And the randomized testing code acts as our ``client'': it lives outside
the module boundaries of each implementation and thus can only interact with
them in approved ways---for example, it has no way to violate the sorting
invariant in @tt{SortedListDict}.

@lab:section{Dictionaries as binary search trees}

Oh snap!---I just thought of an even better way to represent dictionaries.
Searching through a binary search tree (BST) is way faster than searching
through a list, even a sorted one, so let's use that to build a better kind of
dictionary.

@#reader scribble/comment-reader
(racketblock
; A [TreeDict V] is one of:
;  - (td-leaf%)
;  - (td-node% Key V [TreeDict V] [TreeDict V])
;
; Invariant: The keys form a BST.
)

So each key-value mapping is stored at the node of a BST ordered by the keys,
which means that if I'm a node, then my key is bigger than all the keys to my
left and smaller than all the keys to my right. This ordered structure enables
us to find a given key---and thus its value---in @math{O(log n)} time.

Two points to be careful about:
@itemlist[
  @item{Only keys follow the BST ordering; values are just along for the ride.}
  @item{This is different than what we saw in class today.}
]

@exercise{
  Define the classes @racket[td-leaf%] and @racket[td-node%] in a new module
  @racket["tree-dict.rkt"].
}

@exercise{
  Define @tt{IDict} methods on @tt{TreeDict}s. Know your invariant!---make sure
  you both preserve and exploit it.
}

@exercise{
  Define a function @racket[random-tree-dict], similar to @racket[random-list]
  and @racket[random-sorted-list]. (If you abstracted things properly before,
  this should be a one-liner.)
}

@exercise{
  Randomly test your @tt{IList} properties on random @tt{TreeDict}s.
}

@lab:section{More useful dictionaries}

Dictionaries tend to have many more behaviors than the three we've been playing
with so far.

@exercise{
  Add a method @racket[has-value?] to @tt{IDict}s that tests whether a given
  value is present. (Invariants won't help you here!)

  Specify and randomly test a property relating @racket[set] and
  @racket[has-value?].
}

@exercise{
  Add a method @racket[update] to @tt{IDict}s that takes a key @racket[k] and an
  update function @racket[f : V -> V], and updates @racket[k]'s value by
  applying @racket[f] to it. The method should assume that the key @racket[k]
  already exists in the dictionary.

  Specify and randomly test a property relating @racket[update] and
  @racket[lookup].
}

@exercise{
  Add a method @racket[extend] to @tt{IDict}s that takes another @tt{IDict} (who
  knows which kind!) and combines all of its mappings with this' mappings. What
  if the mappings from the two dictionaries overlap?---pick an
  easy-to-understand policy and document it in your contract/purpose.

  Specify and randomly test a property relating @racket[extend] and
  @racket[has-key?].

  Specify and randomly test a property relating @racket[extend] and
  @racket[lookup].
}
