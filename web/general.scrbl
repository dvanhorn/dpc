#lang scribble/manual
@(require "unnumbered.rkt" "utils.rkt")
@;test
@title*{General}

@section*{People}

@bold{Instructors:} 
@link[#:style "plainlink" "http://www.ccs.neu.edu/home/samth/"]{Sam Tobin-Hochstadt}
and 
@link[#:style "plainlink" "http://www.ccs.neu.edu/home/dvanhorn/"]{David Van Horn}.

@bold{TA:} @link[#:style "plainlink" "http://www.ccs.neu.edu/home/dbrown/"]{Dan Brown}

TAs teach labs, supervise the grading of homework sets, hold office
hours, and occasionally substitute in lectures. In general, they are
apprentice teachers and are here to learn how to run a course.

@bold{Tutors:} Nikko Patten, Trevor Sontag, Alex Lee, Jim Shargo

@section*{Office Hours}

@tabular[
  (list (list "Sam Tobin-Hochstadt" (tt "samth") "WVH308 " "Monday" "2:00pm-4:00pm")
        (list "David Van Horn" (tt "dvanhorn") "WVH350" "Thursday " "2:00pm-4:00pm")
        (list "Dan Brown" (tt "dbrown") "WVH308" "Tuesday " "4:00pm-6:00pm")
	(list "Nikko Patten" (tt "npatten")"WVH102" "Wednesday" "11:00am-noon")
	(list "Trevor Sontag" (tt "tasontag") "WVH102" "Wednesday" "7:00pm-8:00pm")
	(list "Alex Lee" (tt "lee") "WVH102" "Wednesday" "10:00pm-11:00pm")
	(list "Jim Shargo" (tt "shargoj") "WVH102" "Monday" "4:00pm-5:00pm"))]

@bold{Communication}: Use CCIS email (@tt{@"@"ccs.neu.edu}) to reach
any of the course staff; usernames are given above.


@bold{Class:} Class consists of lecture (CS2510H) and lab (CS2511H)
sections.

@bold{Lectures:} Mon, Thu 11:45–1:25pm WVH 108

@bold{Labs:} Mon 6:00–7:40pm WVH 210


@section*{Policies}

@itemlist[
  @item{Late policy: there is no late policy; solutions to 
        assignments as they exist at the time of the deadline will be graded.}
  @item{Laptop policy: no laptops in class.}
  @item{Academic honesty: we will strictly enforce Northeastern's
  @link[#:style "plainlink"
  "http://www.northeastern.edu/osccr/academichonesty.html"]{academic
  integrity policy}.  You may discuss problems with other students,
  but you should not share or show code to anyone other than your
  assigned partner.  Violations of academic integrity will be reported
  to OSCCR and will have a negative impact on your grade.}]

@section*{Computing Environment}

We will use @link[#:style "plainlink"
"http://racket-lang.org/"]{DrRacket} v5.0.2.  DrRacket is installed on
the CCS computers. It is also freely available on the web in case you
wish install it on your own computer.

You will need to install our @link[plt-filename]{course software} into DrRacket.
In the DrRacket @tt{File} menu, select @tt{Install PLT File}, and then
enter the url

@tt[(format "http://www.ccs.neu.edu/course/cs2510h/~a" plt-filename)]


You will use @secref{Subversion} to work on your homework sets, to
keep track of revisions, and to submit your homework.

@section*{Problem Sets}

There will be weekly problem sets. 

We will drop the homework grade with the worst impact on your final
grade from consideration for the final grade. Thus, if you just don't
get it one week, nothing is lost. The story is different for the
second or third or ... time.

@section*{Pair Programming}

You must work on your problem sets in pairs. Your lab TA will assign
you a partner. Every few weeks, you will get a new partner.

Pair programming means that you and your partner work on the problem
sets jointly. You read them together and you work on the solutions
together. One of the lab's purposes is to teach you how to work in
pairs effectively; indeed, pairs are provably more effective than
individuals in programming. The rough idea is this: One of you plays
@emph{pilot}, the other @emph{co-pilot}. The pilot works on the
keyboard and explains aloud what is going on; it is the co-pilot's
responsibility to question everything. After a problem is solved to
the satisfaction of both, you @emph{must} switch roles.

@section*{Exams}

@itemlist[
  @item{Exam 1: 2/15 at 6-9 PM}
  @item{Exam 2: 3/22 at 6-9 PM}]

@section*{Projects}

There will be a substantial class project implemented over the last
several weeks of the course.

@section*{Grades}

You will get a gpa for your homework (including the project) and for
your exams. You must have both a passing homework gpa and a passing
gpa to pass the course. For the final grade, we will assign a weight
of 40% to the homework grade and a weight of 55% to the two exams. The
remaining 5% are up to the instructors' whim.
