#lang scribble/book
@(require scribble/manual)

@title[#:tag "book" #:version "" #:style '(toc unnumbered)]{Designing Programs with Class}
@author{Sam Tobin-Hochstadt and David Van Horn}

This book introduces the fundamental elements of class-based program
design.

The book is also available in PDF form @link["dpc.pdf"]{here}.

@table-of-contents[]

@include-section{chapter/00.scrbl}
@section[#:style 'grouper]{Basic Design with Objects}
@include-section{chapter/01.scrbl}
@include-section{chapter/02.scrbl}
@include-section{chapter/03.scrbl}
@include-section{chapter/04a.scrbl}
@include-section{chapter/04b.scrbl}
@section[#:style 'grouper]{Schemes of a Larger Design}
@include-section{chapter/05.scrbl}
@include-section{chapter/universe.scrbl}
@include-section{chapter/guess-my-number.scrbl}
@section[#:style 'grouper]{Abstraction with Objects}
@include-section{chapter/04.scrbl}
@include-section{chapter/07.scrbl}
@include-section{chapter/08.scrbl}
@include-section{chapter/10.scrbl}
@include-section{chapter/12.scrbl}
@section[#:style 'grouper]{Invariants}
@include-section{chapter/09.scrbl}
@include-section{chapter/11.scrbl}
@section[#:tag "Part_Java" #:style 'grouper]{Java}
@include-section{chapter/13.scrbl}
@section[#:style 'grouper]{Equality}
@include-section{chapter/14.scrbl}
@include-section{chapter/15.scrbl}
@include-section{chapter/16.scrbl}
@section[#:style 'grouper]{A Class of Your Own}
@include-section{chapter/17.scrbl}
@section[#:tag "Part_Solutions" #:style 'grouper]{Solutions}
@include-section{solutions.scrbl}
@index-section[]