#lang scribble/manual

@title{Subversion}

@section{Purpose}

@link["http://subversion.tigris.org/"]{Subversion} is a version
control system. The purpose of using such a system is that you can
easily keep track of many different versions of your software
system. In addition, the system helps you work on the same project
from many different places (and in company settings in a distributed
manner).


@section{Your Homework Directories}

You will use subversion to work on your homework sets, to keep track
of revisions, and to submit your homework. In particular, for each
homework set N you will create, add, and commit a directory called
@tt{setN} to your svn repository. For each problem K in a problem set
N, you will create, add, and commit a file called @tt{K.rkt} in
directory @tt{setN}. Thus, all the work for problem set 1 for your
group @tt{GGG} should be visible at

@tt{   https://trac.ccs.neu.edu/svn/cs2510Hspring2011/pairGGG/set1/}

where @tt{GGG} is your pairings index. At any time you can check what
we will see by pointing a web browser to the above url. The browser
will prompt you for a username and a password and, if these are
correct, will display your directory structure as a Web page. Just
like the design recipe asks you to run tests at the end of a design
cycle, you should---no you must---check on your homework assignment on
the server whenever you think you're done.

You should commit all of your work every time you finish working on a
problem. This ensures that we can track problems within pairings, that
you can prove your innocence in terms of code theft, and that you
always have a backup of your work.

@section{Homework submission}

On the due date, an automated script will collect every pair's current
solution at midnight. Since you will commit intermediate solutions,
you are guaranteed to have some solution available at the
deadline. We will grade the code that we collected
on the due date. We will not use a revision that you submit
later. Never.

@section{Additional Information}

Working with subversion is relatively straightforward (for this
course). Check out our
@link["http://www.ccs.neu.edu/home/matthias/107-f09/Assignments/svnguide.pdf"]{local
guide} and pay close attention in the first lab.


