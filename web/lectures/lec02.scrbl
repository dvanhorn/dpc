#lang scribble/manual
@(require scribble/eval
	  "../utils.rkt"
          racket/sandbox
  	  (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class0 define-struct ...))
	  (for-label class0/universe))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    ;(the-eval '(require lang/htdp-intermediate-lambda))
    (the-eval '(require class0))
    (the-eval '(require 2htdp/image))
    (the-eval '(require (prefix-in r: racket)))
    the-eval))

@title[#:tag "lec02"]{1/13: Designing classes}


@section{Announcements}
@itemlist[
@item{Assignment 1 was due last night.
  
  In the future, it is @emph{required} that your Subversion
  directory layout follow the format described
  @seclink["svnlayout"]{on the Subversion page}.  In particular,
  assignment @tt{N} must go in a directory called @tt{assnN}.  
}

@item{Assignment 2 is out and due Wednesday night.
  
  In this assignment, and in the future, when the assignment asks
  you to ``design'' or to ``develop'' a program, you should carry
  out this design using classes and objects.  You can use functions
  where needed, but in this class, our primary design tools will be
  classes, methods, and objects.
  
  @;{Discussion of assignment, playing zombies.}
  
  @;{Do we die when we hit dead zombies?  Up to you.}
  The behavior
  of the game when the player comes into contact with a dead zombie
  is unspecified---you are welcome to add the behavior you prefer.
  Possibilities include the player getting stuck, dying, the zombie
  coming back to life, or whatever you can imagine.
  
  Developing the game will require using the event handling
  facilities of @racketmodname[class0/universe].  See the
  documentation for @racket[on-key] and @racket[on-mouse].
}

@item{If your partner drops the class, please let us know right away,
so that you can be assigned a new partner.  }]

@section{Using a terminal}

Many of you may not have experience using a terminal interface to a
computer, so I first want to walk you through using @tt{svn} in a
terminal.  You certainly don't have to use a terminal, you can
download and install a graphical Subversion client if that's easier
for you.  There are
@link["http://en.wikipedia.org/wiki/Comparison_of_Subversion_clients"]{lots
and lots} of Subversion clients out there, and you're welcome to use
whatever suits your needs best.  However, using the terminal is a
powerful way to interact with a computer and worth knowing.  (Think of
it this way: using graphical interfaces are like communicating by
pointing and gesturing, using textual interfaces such as terminals are
like communicating verbally.  Which would you rather do?)

Here I am going to start by using the terminal that comes with Mac OS
X.  I'm going to check out the course repository, make some changes,
and commit them.  After that, I'll show how you can remotely access
the CCIS computer system through the terminal and do the same thing on
a CCIS Linux machine.

@subsection{Using svn in a terminal on Mac OS X}

So the first thing I'll do is start the Terminal program, which gives
me a prompt, very much like the interactions window in DrRacket.

@verbatim[#:indent 3]{
doom:~ dvanhorn$ pwd
/Users/dvanhorn}

The pwd program prints which directory I am currently in, which is my
home directory (by convention, it's called @tt{~} at the prompt).  I'm
going to make a directory to hold my homework and then move into that
directory.

@verbatim[#:indent 3]{
doom:~ dvanhorn$ mkdir 2510H/
doom:~ dvanhorn$ cd 2510H/
doom:2510H dvanhorn$ pwd
/Users/dvanhorn/2510H}

I can use ls to list the files in this directory:

@verbatim[#:indent 3]{
doom:2510H dvanhorn$ ls}

(There's nothing there yet.)

Now I can check out the subversion repository.

@verbatim[#:indent 3]{
doom:2510H dvanhorn$ svn co https://trac.ccs.neu.edu/svn/cs2510hspring2011/pair014/
A    pair014/assignment0.rkt
Checked out revision 133.}

That added a pair014 directory.  I can verify using ls:

@verbatim[#:indent 3]{
doom:2510H dvanhorn$ ls
pair014}

You only need to do a checkout once; this gives you a local copy of
the repository.  From now on, you can add, modify, and delete files,
then commit them so that your local changes are saved to the
repository. When you want to get the latest version, you can do an
update.

Now I'm going to change into the pair014 directory and see what's there:

@verbatim[#:indent 3]{
doom:2510H dvanhorn$ cd pair014/
doom:pair014 dvanhorn$ ls
assignment0.rkt}

I can check the status of the repository:

@verbatim[#:indent 3]{
doom:pair014 dvanhorn$ svn status}

This doesn't print anything because my local copy has no changes to
the repository.

Now I'm going to open assignment0.rkt and put in a little message to
you.  (I did this in DrRacket.)  I can check the status of the
repository again:

@verbatim[#:indent 3]{
doom:pair014 dvanhorn$ svn status
M       assignment0.rkt}

The "@tt{M}" means that assignment0.rkt has been Modified.  If I want to
take a quick look at the differences between my local copy and the
repository, I can do this:

@verbatim[#:indent 3]|{
doom:pair014 dvanhorn$ svn diff
Index: assignment0.rkt
===================================================================
--- assignment0.rkt    (revision 133)
+++ assignment0.rkt    (working copy)
@@ -1,4 +1,5 @@
 #lang class0
+;; Hi Talia!
 (require 2htdp/image)
 (require class0/universe)
 (+ 1 2)}|

This shows that I edited assignment0.rkt and added (marked with "+")
the line ";; Hi Talia!".  To save the changes in my local copy to the
repository, I need to commit my changes.  When I commit, I need to
give a message about what I'm changing, which I can do with the
@tt{-m} argument to @tt{svn}:

@verbatim[#:indent 3]{
doom:pair014 dvanhorn$ svn commit -m "Added a little message to Talia."
Sending        assignment0.rkt
Transmitting file data .
Committed revision 134.}

Now if I switch to another computer or lose my local copy, I can do
another checkout, or just an update if I already have a local copy,
and be able to get the latest version of the repository.  If you do an
update, for example, you'll see the change I made.  Likewise, your
partner can make changes that you'll see, and you can make changes
your partner will see.

If I want to look at the history of changes to a file, I can do this:

@verbatim[#:indent 3]{
doom:pair014 dvanhorn$ svn log assignment0.rkt
------------------------------------------------------------------------
r134 | dvanhorn | 2011-01-12 16:44:14 -0500 (Wed, 12 Jan 2011) | 1 line

Added a little message to Talia.
------------------------------------------------------------------------
r31 | schwarta | 2011-01-10 18:35:47 -0500 (Mon, 10 Jan 2011) | 1 line

test
------------------------------------------------------------------------}

This shows @tt{assignment0.rkt} has been edited twice; once on Monday
by @tt{schwarta} and once today by me.

Now let me create a new directory and add some files.

@verbatim[#:indent 3]{
doom:pair014 dvanhorn$ mkdir assn1
doom:pair014 dvanhorn$ svn add assn1
A         assn1}

First I made the directory with @tt{mkdir}, then I added it to my
local svn files.  When I commit, this directory will be saved to the
repository.

Now I'll change into the new directory:

@verbatim[#:indent 3]{
doom:pair014 dvanhorn$ cd assn1/}

Now I'll create some files in DrRacket and save them in
@tt{/Users/dvanhorn/2510H/pair014/assn1}.  I can make sure they're
there by using @tt{ls}:

@verbatim[#:indent 3]{
doom:assn1 dvanhorn$ ls
1.rkt    2.rkt}

I still haven't added these files to the repository, so let me do that:

@verbatim[#:indent 3]{
doom:assn1 dvanhorn$ svn add 1.rkt 2.rkt
A         1.rkt
A         2.rkt}

Now let me check the status:

@verbatim[#:indent 3]{
doom:assn1 dvanhorn$ svn status
A       .
A       1.rkt
A       2.rkt}

The "@tt{A}" means a file has been added.  So this response is saying
I've added three files: the current directory, (by convention called
"@tt{.}"), @tt{1.rkt}, and @tt{2.rkt}.

Now I can commit them so they're saved to the repository:

@verbatim[#:indent 3]{
doom:assn1 dvanhorn$ svn commit -m "Added solution to assignment 1."
Adding         assn1
Adding         assn1/1.rkt
Adding         assn1/2.rkt
Transmitting file data ..
Committed revision 135.}

And now we're done.  (You can make sure I added the right thing by
updating your copy of the repository.)  If you want to make changes to
your submission, edit those files then open Terminal, change to the
directory where they live and do an @tt{svn commit -m <your message
here>}.

@subsection{Using svn in a terminal on @tt{login.ccs.neu.edu}}

Now I want to replay this same process, but rather than doing it on my
own machine, I'm going to remotely connect to an Ubuntu (Linux)
machine on the CCIS system and do the work there.  To remotely login
to CCIS, open Terminal do the following:

@verbatim[#:indent 3]|{
doom:~ dvanhorn$ ssh login.ccs.neu.edu
dvanhorn@login.ccs.neu.edu's password: 
Linux login 2.6.31-22-generic #70-Ubuntu SMP Wed Dec 1 23:51:13 UTC 2010 i686

To access official Ubuntu documentation, please visit:
http://help.ubuntu.com/
If you have problems with this system, please report them to
<systems@ccs.neu.edu>.

Regular maintenance is scheduled every Wednesday from 11:45am to 1:45pm
unless otherwise announced, and this machine might not be available
during that time.  See http://www.ccs.neu.edu/downtime/ for details.
Last login: Wed Jan 12 16:09:12 2011 from nomad.ccs.neu.edu
login:~>}|

The @tt{ssh} program is a secure remote login program.  It allows you
to connect to other computers.  Here we told it to connect to the
server @tt{login.ccs.neu.edu}.  After I entered my password, it prints
out a welcome message, and gives me a @emph{different} prompt.  At
this point, I am interacting with the CCIS machine, not my Mac.

First off, unfortunately @tt{svn} does not live in a place the CCIS
machine knows about, so you can't just run it from the command-line
prompt:

@verbatim[#:indent 3]{
login:~> svn
/opt/csw/bin/svn: Command not found.}

But CCIS @emph{does} have the @tt{svn} program; it lives in
@tt{/usr/bin/svn}:

@verbatim[#:indent 3]{
login:~> /usr/bin/svn
Type 'svn help' for usage.}

It's not very convenient to have to always type @tt{/usr/bin/svn}
every time you want to run subversion, so we can set up a shortcut by
making an @emph{alias}:

@verbatim[#:indent 3]{
login:~> alias svn /usr/bin/svn
login:~> svn
Type 'svn help' for usage.}

What I'm saying here is "whenever I say @tt{svn}, the terminal should
interpret that as @tt{/usr/bin/svn}".  This alias will go away if I
log out of the CCIS machine and log back in.  If I want to set up this
convenience for every time I use a CCIS computer, I can edit my
@tt{~/.login} file and add the alias line.

Moving on, now I can do all of the things I did on my Mac.  Here is a
transcript:

@verbatim[#:indent 3]{
login:~> mkdir CS2510H
login:~> cd CS2510H/
login:~/CS2510H> svn co https://trac.ccs.neu.edu/svn/cs2510hspring2011/pair000/
A    pair000/set0
A    pair000/set0/1.rkt
A    pair000/set0/2.rkt
A    pair000/set0/3.rkt
A    pair000/set2
A    pair000/set2/foo.txt
A    pair000/set3
A    pair000/set3/bar.txt
Checked out revision 137.
login:~/CS2510H> cd pair000/set
set0/  set2/  set3/  
login:~/CS2510H> cd pair000/set0/
login:~/CS2510H/pair000/set0> emacs 1.rkt 
login:~/CS2510H/pair000/set0> cd ..
login:~/CS2510H/pair000> svn status
M       set0/1.rkt
login:~/CS2510H/pair000> touch set0/4.rkt
login:~/CS2510H/pair000> svn add set0/4.rkt 
A         set0/4.rkt
login:~/CS2510H/pair000> svn status
M       set0/1.rkt
A       set0/4.rkt
login:~/CS2510H/pair000> svn commit -m "Just messin' around."
Sending        set0/1.rkt
Adding         set0/4.rkt
Transmitting file data ..
Committed revision 138.}

Now if I want to get back to my Mac, I just exit from CCIS:

@verbatim[#:indent 3]{
login:~/CS2510H/pair000> exit
logout
Connection to login.ccs.neu.edu closed.
doom:~ dvanhorn$}



@;$



@section{Designing Classes}

One of the most important lessons of @emph{How to Design Programs} is
that the structure of code follows the structure of the data it
operates on, which means that the structure of your code can be
derived @emph{systematically} from your data definitions.  In this
lecture, we see how to apply the design recipe to design data
represented using classes as well as operations implemented as methods
in these classes. 

@subsection{Atomic and Compound Data}

We saw already in @secref["lec01"] and in @secref["assign01"] how to
design classes that contain multiple pieces of data. Given a class
defined as follows:

@#reader scribble/comment-reader
(racketblock
;; A Posn is (new posn% Number Number)
(define-class posn%
  (fields x y)
  
  ...)
)

the template for a @racket[posn%] method is:

@#reader scribble/comment-reader
(racketblock
 ;; -> ???
 (define (posn%-method)
   ... (field x) ... (field y) ...))

Here we see that our template lists the available parts of the
@racket[posn%] object, in particular the two fields @racket[x] and
@racket[y].

@subsection{Enumerations}

An @deftech{enumeration} is a data definition for a finite set of
possibilities.  For example, we can represent a traffic light like the
ones on Huntington Avenue with a finite set of symbols, as we did in
Fundies I:

@codeblock[#:keep-lang-line? #f]{
#lang class0
;; A Light is one of:
;; - 'Red
;; - 'Green
;; - 'Yellow
}

Following the design recipe, we can construct the template for
functions on @tt{Light}s:

@#reader scribble/comment-reader
(racketblock
 ;; Light -> ???
 (define (light-temp l)
   (cond [(symbol=? 'Red l) ...]
	 [(symbol=? 'Green l) ...]
	 [(symbol=? 'Yellow l) ...]))
 )

Finally, we can define functions over @tt{Light}s, following the template.  

@#reader scribble/comment-reader
(racketblock
 ;; next : Light -> Light
 ;; switch to the next light in the cycle
 (define (next l)
   (cond [(symbol=? 'Red l) 'Green]
	 [(symbol=? 'Green l) 'Yellow]
	 [(symbol=? 'Yellow l) 'Red]))
 (check-expect (next 'Green) 'Yellow)
 (check-expect (next 'Red) 'Green)
 (check-expect (next 'Yellow) 'Red))

That's all well and good for a function-oriented design, but we want
to design this using classes, methods, and objects.


There are two obvious possibilities.  First, we could create a
@racket[light%] class, with a field holding a @racket[Light].
However, this fails to use classes and objects to their full
potential.  Instead, we will design a class for each state the traffic
light can be in.  Each of the three classes will have their own
implementation of the @racket[next] method, producing the appropriate
@tt{Light}.

@#reader scribble/comment-reader
(racketmod
  class0
  ;; A Light is one of:
  ;; - (new red%)
  ;; - (new green%)
  ;; - (new yellow%)

  (define-class red%
    ;; -> Light
    ;; Produce the next traffic light
    (define/public (next)
      (new green%)))

  (define-class green%
    ;; -> Light
    ;; Produce the next traffic light
    (define/public (next)
      (new yellow%)))

  (define-class yellow%
    ;; -> Light
    ;; Produce the next traffic light
    (define/public (next)
      (new red%)))

  (check-expect (send (new red%) next) (new green%))
  (check-expect (send (new green%) next) (new yellow%))
  (check-expect (send (new yellow%) next) (new red%))
)


If you have a @tt{Light} @racket[L], how do you get the next light?

@racket[(send L next)]

Note that there is no use of @racket[cond] in this program, although
the previous design using functions needed a @racket[cond].  Instead,
the @racket[cond] is happening behind your back, because the object
system picks the appropriate @racket[next] method to call.


@section{Unions and Recursive Unions}

@deftech{Unions} are a generalization of enumerations to represent
infinite families of data.  One example is @emph{binary trees}, which can contain arbitrary other data as elements.  We'll now look at how to model binary trees of numbers, such as:

@verbatim[#:indent 2]{
          7         6              8  
     	           / \            / \ 
                  8   4          2   1
	             / \ 
	            3   2}

How would we represent this with classes and objects?

@#reader scribble/comment-reader
(racketmod
class0
;; A BT is one of:
;; - (new leaf% Number)
;; - (new node% Number BT BT)
(define-class leaf%
  (fields number))

(define-class node%
  (fields number left right))

(define ex1 (new leaf% 7))
(define ex2 (new node% 6
		 (new leaf% 8)
		 (new node% 4
		      (new leaf% 3)
		      (new leaf% 2))))
(define ex3 (new node% 8
		 (new leaf% 2)
		 (new leaf% 1)))
)

We then want to design a method @racket[count] which produces the
number of numbers stored in a @tt{BT}.  

Here are our examples:

@racketblock[
(check-expect (send ex1 count) 1)
(check-expect (send ex2 count) 5)
(check-expect (send ex3 count) 3)
]

Next, we write down the
templates for methods of our two classes.

The template for @racket[leaf%]:
@#reader scribble/comment-reader
(racketblock
;; -> Number
;; count the number of numbers in this leaf
(define/public (count)
 ... (field number) ...)
)

The template for @racket[node%]:
@#reader scribble/comment-reader
(racketblock
;; -> Number
;; count the number of numbers in this node
(define/public (count)
 ... (field number) ...
 (send (field left) count) ...
 (send (field right) count) ...)
)


Now we provide a definition of the @racket[count] method for each of
our classes.

For @racket[leaf%]:
@#reader scribble/comment-reader
(racketblock
;; -> Number
;; count the number of numbers in this leaf
(define/public (count)
  1)
)

For @racket[node%]:
@#reader scribble/comment-reader
(racketblock
;; -> Number
;; count the number of numbers in this node
(define/public (count)
  (+ 1
     (send (field left) count)
     (send (field right) count)))
)


Next, we want to write the @racket[double] function, which takes a
number and produces two copies of the @tt{BT} with the given number at
the top.  Here is a straightforward implementation for @racket[leaf%]:

@#reader scribble/comment-reader
(racketblock
;; Number -> BT
;; double the leaf and put the number on top
(define/public (double n)
  (new node%
       n
       (new leaf% (field number))
       (new leaf% (field number))))
)

Note that @racket[(new leaf% (field number))] is just constructing a
new @racket[leaf%] object just like the one we started with.
Fortunately, we have a way of referring to ourselves, using the
identifier @racket[this].  We can thus write the method as:

@#reader scribble/comment-reader
(racketblock
;; Number -> BT
;; double the leaf and put the number on top
(define/public (double n)
  (new node% n this this))
)

For @racket[node%], the method is very similar:
@margin-note{Since these two methods are so similar, you may wonder if
they can be abstracted to avoid duplication.  We will see how to do
this in a subsequent class.}

@#reader scribble/comment-reader
(racketblock
;; Number -> BT
;; double the node and put the number on top
(define/public (double n)
  (new node% n this this))
)


The full @tt{BT} code is now:
@#reader scribble/comment-reader
(racketmod
class0
;; A BT is one of:
;; - (new leaf% Number)
;; - (new node% Number BT BT)

(define-class leaf%
  (fields number)
  ;; -> Number
  ;; count the number of numbers in this leaf
  (define/public (count)
  1)
  ;; Number -> BT
  ;; double the leaf and put the number on top
  (define/public (double n)
    (new node% n this this)))

(define-class node%
  (fields number left right)
  ;; -> Number
  ;; count the number of numbers in this node
  (define/public (count)
    (+ 1
       (send (field left) count)
       (send (field right) count)))
  ;; Number -> BT
  ;; double the node and put the number on top
  (define/public (double n)
    (new node% n this this)))

(define ex1 (new leaf% 7))
(define ex2 (new node% 6
		 (new leaf% 8)
		 (new node% 4
		      (new leaf% 3)
		      (new leaf% 2))))
(define ex3 (new node% 8
		 (new leaf% 2)
		 (new leaf% 1)))

(check-expect (send ex1 count) 1)
(check-expect (send ex2 count) 5)
(check-expect (send ex3 count) 3)

(check-expect (send ex1 double 5)
	      (new node% 5 ex1 ex1))
(check-expect (send ex3 double 0)
	      (new node% 0 ex3 ex3))
)
