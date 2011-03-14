#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt")

@(define exercise (exercise-counter))

@title[#:tag "lab10"]{3/14: A little Java}

@itemlist[
  @item{Recall the Java basics we covered in lecture @secref["lec17"].}
  @item{Download @tt{prima-1.2.3.jar} from
        @url{http://code.google.com/p/nutester}.}
]

@lab:section{Same data, different language}

Here are data and interface definitions for lists of integers in Java. Notice
that it's not much different than what we'd write in Racket.

@indented{@verbatim|{
  // An IntList is one of:
  //  - new EmptyIntList()
  //  - new ConsIntList(Integer, IntList)
  //
  // and implements:
  //
  // length : -> Integer
  // The length of the list
  //
  // first : -> Integer
  // The first element in the list (assumes list is non-empty)
  //
  // rest : -> IntList
  // The rest of the list (assumes list is non-empty)
  //
  // filterEvens : -> IntList
  // Just the even numbers
  //
  // squares : -> IntList
  // Square every number in the list
  //
  // sum : -> Integer
  // Sum the numbers in the list
}|}

@exercise{
  Define the @racket[IntList] interface and the @racket[EmptyIntList] and
  @racket[ConsIntList] classes that implement it.

  In the @racket[first] and @racket[rest] methods for @racket[EmptyIntList], you
  can say @tt{throw new Exception("blah blah")} to signal an error.
}

Let's do it again, but without committing ourselves to @racket[Integer]s.

@indented{@verbatim|{
  // A List<X> is one of:
  //  - new Empty<X>()
  //  - new Cons<X>(X, List<X>)
  //
  // and implements:
  //
  // length : -> Integer
  // The length of the list
  //
  // first : -> X
  // The first element in the list (assumes list is non-empty)
  //
  // rest : -> List<X>
  // The rest of the list (assumes list is non-empty)
}|}

@exercise{
  Define the @racket[List<X>] interface and the @racket[Empty<X>] and
  @racket[Cons<X>] classes that implement it.
}

@exercise{
  Your @racket[List<X>], @racket[Empty<X>], and @racket[Cons<X>] should be very
  similar to your @racket[IntList], @racket[EmptyIntList], and
  @racket[ConsIntList]. Eliminate the redundancy by changing @racket[IntList]s
  to inherit from @racket[List<Integer>]s: the @racket[IntList] interface should
  inherit from the @racket[List<Integer>] interface, and the
  @racket[EmptyIntList] and @racket[ConsIntList] classes should inherit from the
  @racket[Empty<Integer>] and @racket[Cons<Integer>] classes, respectively.

  Recall that, in Java, an interface @racket[extends] another interface to
  inherit from it, a class @racket[extends] another class to inherit from it,
  and a class @racket[implements] an interface to implement it.

  Since the @racket[length], @racket[first], and @racket[rest] methods are now
  inherited, remove the unnecessary definitions from @racket[IntList],
  @racket[EmptyIntList], and @racket[ConsIntList].
}

Java doesn't have higher-order functions, but we can simulate them with objects.
Here's an interface defintion for a unary function from @tt{X} to @tt{Y}:

@indented{@verbatim|{
  // A Fun<X,Y> implements:
  //
  // call : X -> Y
  // Call this function on its X input and produce a Y
}|}

And a sub-interface for unary predicates:

@indented{@verbatim|{
  // A Pred<X> implements Fun<X,Boolean>
}|}

@exercise{
  Define the @racketidfont{Fun<X,Y>} and @racket[Pred<X>] interfaces, and add
  the following method to @racket[List<X>]s:

  @indented{@verbatim|{
    // filter : Pred<X> -> List<X>
    // Only the elements satisfying the predicate
  }|}

  Simplify @racket[filterEvens] on @racket[IntList]s by reimplementing it in
  terms of the inherited @racket[filter] method.

  Instead of writing down a little @racket[λ] to construct your even predicate,
  you'll need to define a new class at the top level and then construct an
  instance of it to get your predicate object. (Java does have a better way to
  do this, but it would be too much of a digression to explain it. Many other
  languages---including C#---just give you @racket[λ].)
}

@exercise{
  Add the following method to @racket[List<X>]s:

  @indented{@verbatim|{
    // map : Fun<X,Y> -> List<Y>
    // The list where every element has been mapped through the given function
  }|}

  Simplify @racket[squares] on @racket[IntList]s with @racket[map]. You'll again
  need to define a new top-level class for the squaring function.
}

Here's an interface definition for binary functions:

@indented{@verbatim|{
  // A Fun2<X,Y,Z> implements:
  //
  // call : X Y -> Z
  // Call this function on its X and Y inputs and produce a Z
}|}

@exercise{
  Add the following method to @racket[List<X>]s:

  @indented{@verbatim|{
    // foldr : Fun2<X,Y,Y> Y -> Y
    // Fold the list from the right
  }|}

  Simplify @racket[sum] on @racket[IntList]s with @racket[foldr]. (Again, you'll
  need a top-level class for the adding function.)
}

@racket[ConsIntList] and @racket[EmptyIntList] are currently the only kinds of
@racket[IntList], but it's not necessary to keep it that way. Here's another
kind of @racket[IntList] that keeps itself sorted as you @racket[insert]
elements into it:

@indented{@verbatim|{
  // A SortedIntList is one of:
  //  - new SortedEmptyIntList()
  //  - new SortedConsIntList(Integer, SortedIntList)
  // such that:
  //   Invariant: numbers occur in non-decreasing order
  //
  // A SortedIntList implements IntList, in addition to:
  //
  // insert : Integer -> SortedIntList
  // Insert the given number
}|}

@exercise{
  Define the @racket[SortedIntList] interface and the
  @racket[SortedEmptyIntList] and @racket[SortedConsIntList] classes. Your
  @racket[SortedIntList] interface should extend the @racket[IntList] interface,
  and your classes should implement the @racket[SortedIntList] interface.

  Be sure to maintain the @racket[SortedIntList] invariant!

  To avoid writing unnecessary code, have your two classes inherit from
  @racket[EmptyIntList] and @racket[ConsIntList], respectively. Do you need to
  overload any of the inherited methods to avoid violating the sorting
  invariant?
}

@exercise{
  Add the following method to (unsorted) @racket[IntList]s:

  @indented{@verbatim|{
    // isElement : Integer -> Boolean
    // Whether the given element is present in the list
  }|}

  @racket[SortedIntList]s inherit this method for free, but we can write a
  better one if we take advantage of the sorting invariant. Override
  @racket[isElement] for @racket[SortedIntList]s so that they can stop searching
  partway through the list.
}

The sorting invariant above is somewhat helpful, but when we want to get serious
about searching data we need to move away from linear structures and consider
something tree shaped, like this new kind of @racket[SortedIntList]:

@indented{@verbatim|{
  // An IntBST (integer binary search tree) is one of:
  //  - new Leaf()
  //  - new Node(Integer, IntBST, IntBST)
  // such that:
  //   Invariant: no number on the left is bigger than any number on the right
  //
  // an IntBST implements SortedIntList.
  //
  // A BST represents a list as follows:
  //  - the first element in the list is the left-most element in the tree
  //  - the rest of the list is everything but the left-most element in the tree
}|}

The advantage of the tree representation is that methods like @racket[isElement]
can be implemented more intelligently: instead of examining every element, we
can rely on the search-tree invariant to ignore entire branches of the tree as
we descend down it looking for the given element.

@exercise{
  Define the @racket[IntBST] interface and the @racket[Leaf] and @racket[Node]
  classes. The @racket[IntBST] interface should inherit from the
  @racket[SortedIntList] interface, but there's nothing useful for the
  @racket[Leaf] and @racket[Node] classes to inherit from, so you have a bit of
  work to do to implement all the required methods.

  Be sure you take advantage of the search-tree invariant when implementing
  @racket[isElement].

  Implementing @racket[first] and @racket[rest] is slightly tricky: consider
  adding an @racket[isLeaf] method to @racket[IntBST]s to help you here.
}
