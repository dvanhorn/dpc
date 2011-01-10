#lang scribble/manual
@(require "unnumbered.rkt"
          "utils.rkt")

@(define (ittt . args) (italic (apply tt args)))

@title*{Subversion}

In this course we will use @link["http://subversion.tigris.org/"]{Subversion}
for collaboration, version control, and homework submission. This guide and the
@seclink["lab01"]{first lab} will show you everything you'll need to use
Subversion in this course.

@section*{Purpose}

Subversion is a version control system that keeps versions of your files in a
"repository", which we will provide for you on a server. The repository will
enable you to retrieve ("checkout", "update") and store ("commit") your files
from multiple locations, and it will enable us to easily access your code for
grading. It also provides a backup mechanism for you in case you ever need to
revert to older versions of your code.

@section*{Your homework directories}

You will use subversion to (1) work on your homework sets, (2) keep track of
revisions, and (3) submit your homework. For each homework set @emph{N} create a
directory @tt{setN} containing each problem in a separate racket file named
@tt{K.rkt}, where @emph{K} is the problem number. For example, if homework set 1
has three problems, your directory should look like this:

@indented[@verbatim{
  set1
   - 1.rkt
   - 2.rkt
   - 3.rkt
}]

Your work will always be visible within the repository at

@indented[
  @tt{https://trac.ccs.neu.edu/svn/cs2510hspring2011/pairP/}
]

where @emph{P} identifies your pair. So the current state of your work for
homework set 1, as far as the repository is concerned, would be available at

@indented[
  @tt{https://trac.ccs.neu.edu/svn/cs2510hspring2011/pairP/set1/}
]

If you point your browser at the above url you will see your submitted directory
from the repository's point of view. Just like the design recipe asks you to run
tests at the end of a design cycle, you should check on your homework assignment
on the server whenever you think you're done.

You should commit all of your work every time you finish working on a
problem. This ensures that we can track problems within pairings, that
you can prove your innocence in terms of code theft, and that you
always have a backup of your work.

@section*{Homework submission}

On the due date, an automated script will collect every pair's current solution
at midnight. Each time you commit an intermediate solution you're guaranteeing
yourself to have something available at the deadline. We will grade the code
that we collect at midnight and ignore any revisions that you submit later. No
exceptions.

@section*{Getting Subversion}

The Subversion client may be downloaded at:

@indented[
  @url{http://subversion.tigris.org}
]

Binaries for several systems such as Windows, Mac OS X, and Linux are
available.  If you have Windows, for example, you should the follow
the link to ``Windows binaries.''  If you have Linux or Mac OS X,
Subversion is likely pre-installed.

You can confirm your Subversion installation
by running it at your system's command prompt:

@indented[@verbatim{
  $ svn help
  usage: svn <subcommand> [options] [args]
  Subversion command-line client, version 1.6.5.
  ...
}]

@section*{Command reference}

Subversion commands are of the form:

@indented{@tt{
  $ svn @ittt{verb} [@ittt{adverbs}] [@ittt{nouns}]
}}

The @ittt{verb} says @emph{what} to do, such as to @tt{update} or
@tt{add} information on a file.  The optional @ittt{adverbs} say
@emph{how} to do it, for example @tt{-q} for quietly or
@tt{-N} for non-recursively.  The optional @ittt{nouns} are usually files or
URLs on which to act.

@subsection*{Essential verbs}

There are two useful verbs that ask for information:

@itemlist[
  @item{@tt{help [@ittt{verb}]}

  Display information for how to use @tt{svn}. If @ittt{verb} is given, display
  information for how to use @tt{svn @ittt{verb}}.}

  @item{@tt{status} [@ittt{files and directories}]

  Display information about changes since your last commit to the repository.
  (Also see @tt{log} and @tt{diff}.)}]

There are various verbs for telling Subversion which files in your local
directory it is responsible for. Here's the most common one:

@itemlist[
  @item{@tt{add [@ittt{files and directories}]}

  Places @ittt{files and directories} under Subversion's control: the @emph{next
  time you commit} they will be sent to the repository. (Also see @tt{rm},
  @tt{mkdir}, @tt{mv}, and @tt{cp}.)}]

And there are several verbs for talking to the repository:

@itemlist[

  @item{@tt{checkout @ittt{repository-url} [@ittt{new-directory}]}

  Makes a local copy of a repository.  You only need to do
  this once per computer you want to work on.}

  @item{@tt{update [@ittt{files and directories}]}

  Updates your local files with changes from the repository, merging if
  necessary.}

  @item{@tt{commit [@ittt{files and directories}]}

  Push changes from the local copy into a new revision on the repository. Use
  the @tt{-m} switch to give the revision a short description.

  Subversion will refuse to commit if the repository has newer information than
  your local copy. In this case, @tt{update} and try again.}]
