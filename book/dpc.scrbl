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
@include-section{chapter/objects.scrbl}
@include-section{chapter/data-defns.scrbl}
@include-section{chapter/interface-defns.scrbl}
@include-section{chapter/04a.scrbl}
@include-section{chapter/solidify.scrbl}
@section[#:style 'grouper]{Schemes of a Larger Design}
@include-section{chapter/05.scrbl}
@include-section{chapter/universe.scrbl}
@include-section{chapter/guess-my-number.scrbl}
@section[#:style 'grouper]{Abstraction with Objects}
@include-section{chapter/delegation.scrbl}
@include-section{chapter/inheritance.scrbl}
@include-section{chapter/function-objects.scrbl}
@include-section{chapter/overriding.scrbl}
@include-section{chapter/extensional.scrbl}
@include-section{chapter/visitors.scrbl}
@section[#:style 'grouper]{Invariants}
@include-section{chapter/invariants.scrbl}
@include-section{chapter/constructors.scrbl}
@section[#:style 'grouper]{Mutation}
@include-section{chapter/mutate.scrbl}
@include-section{chapter/circular.scrbl}
@include-section{chapter/back-channel.scrbl}
@include-section{chapter/intensional.scrbl}
@section[#:tag "Part_Java" #:style 'grouper]{Java}
@include-section{chapter/13.scrbl}
@include-section{chapter/java-types.scrbl}
@include-section{chapter/java-extensional.scrbl}
@section[#:style 'grouper]{A Class of Your Own}
@include-section{chapter/17.scrbl}
@section[#:tag "Part_Solutions" #:style 'grouper]{Solutions}
@include-section{solutions.scrbl}
@index-section[]