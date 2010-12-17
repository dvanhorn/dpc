#lang scribble/manual
@(require "unnumbered.rkt")

@(define (ittt . args) (italic (apply tt args)))

@title{Subversion}

In this course, we will
@link["http://subversion.tigris.org/"]{Subversion} for collaboration,
version control, and homework submission.

Working with subversion is relatively straightforward (for this
course). Read this local guide closely and pay close attention in the
@seclink["lab01"]{first lab}.

@section{Purpose}

Subversion is a version control system that keeps versions of your
files in a "repository."  For this course, we provide a repository on
a remote server for you to use.  This enables you to retrieve
("checkout," "update") and store ("commit") your files from multiple
locations and enables us to access your code for grading.  It also
provides backup and a means for reverting to older revisions if
necessary.

The purpose of using such a system is that you can easily keep track
of many different versions of your software system. In addition, the
system helps you work on the same project from many different places
(and in company settings in a distributed manner).

@section{Your homework directories}

You will use subversion to work on your homework sets, to keep track
of revisions, and to submit your homework. In particular, for each
homework set @emph{N} you will create, add, and commit a directory
called @tt{setN} to your svn repository. For each problem @emph{K} in
a problem set @emph{N}, you will create, add, and commit a file called
@tt{K.rkt} in directory @tt{setN}. Thus, all the work for problem set
1 for your group @tt{GGG} should be visible at

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

@section{Getting Subversion}

The Subversion client may be downloaded at:

@url{http://subversion.tigris.org/}

Binaries for several systems such as Windows, Mac OS X, and Linux are
available.  If you have Windows, for example, you should the follow
the link to ``Windows binaries.''  If you have Linux or Mac OS X,
Subversion is likely pre-installed.

You can confirm your Subversion installation
by running it at your system's command prompt:

@verbatim[#:indent 2]{
$ svn help
usage: svn <subcommand> [options] [args]
Subversion command-line client, version 1.6.5.
...}

@section{Command reference}

Subversion commands are of the form:

@commandline{
$ svn @ittt{verb} [@ittt{adverbs}] [@ittt{nouns}]
}

The @ittt{verb} says @emph{what} to do, such as to @tt{update} or
@tt{add} information on a file.  The optional @ittt{adverbs} say
@emph{how} to do it, for example @tt{-q}uietly or
@tt{-N}on-recursively.  The optional @ittt{nouns} are usually files or
URLs on which verb should act.

@subsection{Essential verbs}

Two important Subversion verbs are for displaying information:

@itemlist[ 
  @item{@tt{help [@ittt{verb}]}

   Display usage information for @italic{@tt{verb}}.}

  @item{@tt{status} [@ittt{files or directories}]

  Display information about changes since your last commit.  (See
  also: @tt{log} and @tt{diff}.)}]

Subversion has several verbs for keeping track of which files it is to
manage.

@itemlist[ 
  @item{@tt{add [@ittt{files or directories}]}

  Places @ittt{files or directories} under Subversion's control.
  (This has no effect on the repository until you commit!  See also:
  @tt{rm}, @tt{mkdir}, @tt{mv}, and @tt{cp}.)}]

There are several verbs for working with the repository:

@itemlist[
  
  @item{@tt{checkout @ittt{repository-url} [@ittt{new-directory}]}
  
  Makes a new working copy from a repository.  You only have to do
  this when you start working on a new machine.}

  @item{@tt{update [@ittt{files or directories}]}

  Grabs the latest version from the repository and merges with your
  working copy.}

  @item{@tt{commit [@ittt{files or directories}]}

   Saves changes from the working copy to a new revision in the
  repository.  The @tt{-m} switch may be useful here.

  Attempting to commit will fail if the repository has changed since
  your last update.  Since you are pair programming in this course,
  merge conflicts should not be a problem.}]
  
