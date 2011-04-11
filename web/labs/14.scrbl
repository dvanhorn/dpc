#lang scribble/manual

@(require "../lab.rkt"
          "../unnumbered.rkt"
          "../utils.rkt")

@(define exercise (exercise-counter))

@title[#:tag "lab14"]{4/11: A little Ruby}

To get us oriented, here is an example Ruby program:

@indented{@verbatim|{
  require 'test/unit'

  # A List(X) implements
  #
  # first : -> X
  # The first element in a non-empty list
  #
  # rest : -> List(X)
  # The rest of a non-empty list
  #
  # length : -> Number
  # The length of a list

  # An Empty is an Empty.new()
  # and implements List(X)
  class Empty

    def length()
      0
    end

  end

  # A Cons(X) is a Cons.new(X, List(X))
  # and implements List(X)
  class Cons

    def initialize(first, rest)
      @first = first
      @rest  = rest
    end

    def first()
      @first
    end

    def rest()
      @rest
    end

    def length()
      1 + @rest.length()
    end

  end

  class ListTests < Test::Unit::TestCase

    def test_first_rest()
      assert_equal(Cons.new(1, Cons.new(2, Empty.new())).first(), 1)
      assert_equal(Cons.new(1, Cons.new(2, Empty.new())).rest().first(), 2)
    end

    def test_length()
      assert_equal(Cons.new(1, Cons.new(2, Empty.new())).length(), 2)
      assert_equal(Cons.new(2, Empty.new()).length(), 1)
      assert_equal(Empty.new().length(), 0)
    end

  end

  xs = Cons.new(1, Cons.new(2, Empty.new()))
  puts "xs is #{xs}"
  puts "The length of xs is #{xs.length()}"
}|}

Classes and methods look familiar. Constructors are spelled @tt{initialize} and
are run for effect only. Fields are written like @tt|{@first}| and aren't
explicitly declared anywhere---they're simply assigned on a whim, typically in
the constructor, at which point they come into existance. Fields are always
private, and it's common to define a @emph{getter} method of the same name to
access the field.

The testing framework is loaded by @tt{require 'test/unit'}. After your file
loads, it finds and instantiates all subclasses of @tt{Test::Unit::TestCase} and
runs all of their methods beginning with @tt{test}. This is why you see its
output after the output from the two @tt{puts}.

@exercise{
  Run the program above. For example, copy it into a file named
  @tt{example-1.rb} and run it with the command:

  @indented{@verbatim{
    $ ruby example-1.rb
  }}
}

@lab:section{Parentheses}

Parentheses are optional in Ruby, except when they're not. (I'm not going to
pretend I know
@link["http://books.google.com/books?id=jcUbTcr5XWwC&pg=PA183&lpg=PA183&dq=ruby+optional+parens&source=bl&ots=fIAiwe5ufz&sig=bAjT5lkTSsrwhSrft5G33mqM7WE&hl=en&ei=BmOjTfmiNMbWsgaM19zqAQ&sa=X&oi=book_result&ct=result&resnum=4&sqi=2&ved=0CDEQ6AEwAw#v=onepage&q=ruby%20optional%20parens&f=false"]{the
rules}). Here's the same code with a bunch of parens removed:

@indented{@verbatim|{
  require 'test/unit'

  # A List X implements
  #
  # first : -> X
  # The first element in a non-empty list
  #
  # rest : -> List X
  # The rest of a non-empty list
  #
  # length : -> Number
  # The length of a list

  # An Empty is an Empty.new
  # and implements List X
  class Empty

    def length
      0
    end

  end

  # A Cons X is a Cons.new X, List(X)
  # and implements List X
  class Cons

    def initialize first, rest
      @first = first
      @rest  = rest
    end

    def first
      @first
    end

    def rest
      @rest
    end

    def length
      1 + @rest.length
    end

  end

  class ListTests < Test::Unit::TestCase

    def test_first_rest
      assert_equal Cons.new(1, Cons.new(2, Empty.new)).first, 1
      assert_equal Cons.new(1, Cons.new(2, Empty.new)).rest.first, 2
    end

    def test_length
      assert_equal Cons.new(1, Cons.new(2, Empty.new)).length, 2
      assert_equal Cons.new(2, Empty.new).length, 1
      assert_equal Empty.new.length, 0
    end

  end

  xs = Cons.new 1, Cons.new(2, Empty.new)
  puts "xs is #{xs}"
  puts "The length of xs is #{xs.length}"
}|}

Notice that zero-argument methods never need parens, which makes them look as if
they were field accesses, and the outermost method call never needs parens.

Use parens however you like; it's common in Ruby to drop as many as possible.

@lab:section{Finger exercises}

@exercise{
  Define an @tt{append} method on Lists.
}

@exercise{
  Define a @tt{reverse} method on Lists.
}

@exercise{
  Define a @tt{Posn} class and construct an example @tt{List Posn}.
}

@lab:section{Higher-order computation: Methods, functions, and blocks}

@indented{@verbatim|{
  class A
    def f
      'Hi from f!'
    end
  end

  def g
    'Hi from g!'
  end
}|}

In the code above, @tt{f} is a method on the class @tt{A}. What is @tt{g}?---it
behaves like a top-level function in Racket, but in Ruby it's in fact a method
that lives on the @emph{top-level class}. (Ok, it's not actually a class, it's a
module---but they're @link["http://www.rootr.net/rubyfaq-8.html#ss8.8"]{very
similar}.)

As we saw in class, we can't pass methods around like we would expect to be able
to with functions. In fact, this is no different than the @racket[class]
languages: to pass a method to someone else we had to wrap it in a @racket[λ]
and turn it into a function, which we could pass around.

Ruby distinguishes methods from functions: methods are the behaviors that
objects respond to, and functions are computations that can be passed around.
Both objects and functions are forms of higher-order computation. Functions look
like this:

@indented{@verbatim|{
  h = lambda { 42 }
  puts h.call

  k = lambda { |x| x * x }
  puts k.call 10
  puts k.call 10 * 2
  puts k.call(10 * 2)
  puts k.call(10) * 2

  l = lambda { |f,x| f.call f.call(x) }
  puts l.call k, 10
}|}

Instead of curly braces you can also write @tt{do ... end}:

@indented{@verbatim|{
  lambda do 42 end

  lambda do |x| x * x end

  lambda do |f,x| f.call f.call(x) end

  lambda do |f,x|
    f.call f.call(x)
  end

  lambda do |f,x|
    puts "Oh boy..."
    f.call f.call(x)
    puts "Just called f twice!"
  end
}|}

In addition to methods and functions, Ruby also has the concept of
@emph{blocks}. They are like functions but less useful: they can only be passed
as the last argument to a method---this is such a common pattern that blocks end
up being more convenient than functions in many cases.

Here's function passing:

@indented{@verbatim|{
def twice_fun x, f
  f.call f.call(x)
end

puts twice_fun 2, lambda {|x| x*x}
}|}

And here's block passing:

@indented{@verbatim|{
def twice_block x
  yield yield(x)
end

puts twice_block(2) { |x| x*x }

puts twice_block(2) { |x|
  x*x
}

puts twice_block 2 do |x| x*x end

puts twice_block 2 do |x|
  x*x
end
}|}

The method @tt{twice_fun} takes two arguments, the second of which is a
function. The method @tt{twice_block} takes @emph{one} argument, along with an
implicit block, which it can call with the @tt{yield} keyword.

Notice that we used parens when passing a block spelled like @tt{{ ... }}, but
we omitted them when passing a block spelled like @tt{do ... end}. This is
because the two ways of spelling blocks have different @emph{operator
precedence}, a topic we won't spend our time discussing here. Rule of thumb: if
the block follows other arguments (like above), use @tt{do ... end}; if the
block is the only argument (unlike above), use whichever you like.

One last thing. If you want to stuff your incoming block into a list or
something, you'll need to give it a name. You can do that with yet another kind
of syntax that turns it into a function:

@indented{@verbatim|{
def twice_block x, &f
  f.call f.call(x)
end
}|}

Whew.

Let's gather ourselves.
@itemlist[
  @item{Methods are how objects respond to message. Methods can't be passed
  around; objects can.}
  @item{Blocks are computation that can be passed around, but only as the last
  argument to a method.}
  @item{Functions are objects that respond to the @tt{call} message. The
  @tt{lambda} syntax turns a block into a function (object).}
]

None of these concepts are new. They're just organized differently.

@exercise{
  Reflect.
}

@lab:section{Some practice with Ruby}

@exercise{
  Write a method @tt{map : (X => Y) -> List Y} on @tt{List X} that maps a block
  down the list. We will write @tt{=>} for blocks.

  Write a method @tt{map_fun : (X -> Y) -> List Y} on @tt{List X} that maps a
  function down the list. We will continue writing @tt{->} for methods and
  functions.
}

@exercise{
  Write a method @tt{filter : (X => Boolean) -> List X} on @tt{List X} that
  filters a list by a predicate (passed as a block).
}

@exercise{
  Write a method @tt{foldr : Y, (X,Y => Y) -> Y} on @tt{List X} that folds a
  list right to left. Note that the value replacing @racket[empty] is the only
  argument to the method, and the computation replacing @racket[cons] is the
  (implicit) block.
}

The kind of lists that are built in to Ruby are called @tt{Array}s and look like
this:

@indented{@verbatim|{
puts [1,2,3,4]
puts [1,2,3,4].map { |x| x ** 2 }
puts [1,2,3,4].select { |x| x % 2 == 0 }
puts [1,2,3,4].reduce(1) { |x,y| x * y }
}|}

Arrays have @link["http://ruby-doc.org/core-1.8.7/classes/Array.html"]{lots of
methods}.

@exercise{
  Write a top-level method that computes the sum of the numbers in a list.
}

@exercise{
  Write a top-level method that tests whether all numbers in a list are
  squares.
}

@exercise{
  Write a top-level method that merges two sorted lists into a bigger sorted
  list.
}

Another common structure we've seen is @emph{dictionaries}, which map keys to
values. In Ruby, they're called @tt{Hash}es, and they look like this:

@indented{@verbatim|{
d = { 'one' => 1, 'two' => 2 }
puts d['one']
puts d['two']
puts d['oops']
}|}

Hashes also have @link["http://ruby-doc.org/core-1.8.7/classes/Hash.html"]{lots
of methods}.

@exercise{
  Write a top-level method that takes a hash and tests whether any two of its
  keys map to the same value.

  Do any of the built-in @tt{Hash} methods help you?
}

@exercise{
  Write a top-level method that takes an array and produces a hash such that (1)
  each element of the array is a a key in the hash and (2) each key maps to the
  number of times it occurred in the array.
}

@exercise{
  Write a top-level method that takes an array and produces a new array with all
  duplicates removed.

  You could do this by scanning the list once per element, but if you use a hash
  you can do it in only one pass...
}
