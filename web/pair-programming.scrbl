#lang scribble/manual
@(require "unnumbered.rkt")

@title*[#:tag "pair"]{Pair Programming}

Pair programming emerged in the 1990s in several different settings. 
Companies realized that when individual programmers left, they were 
left with a team that lacked knowledge of critical pieces of software.
Academics began to understand the importance of articulating technical 
ideas and discussing programs for the design process.

Roughly speaking, pair programming is a collaboration technique. As 
the name says, you and a partner design programs together. In practice, 
one of you---called the pilot---is at the keyboard and the other one---the
co-pilot---is looking over the pilot's shoulder, reading and analyzing and
questioning what the pilot types. To make this work, the pilot must explain
what he is doing, somewhat in a stream of consciousness fashion. When a 
defined sub-problem is solved, the partners switch roles to counter-act 
fatigue and to distribute knowledge uniformly.

Experience suggests that pair programming is more effective than individual
programming, even though it imposes its own costs on the process. To 
understand this point, Williams and Kessler conducted an extensive study 
of pair programming practices in the late 1990s. Academia benefits from pair
programming because programmers train each other making it less likely that 
an individual hangs on to a misunderstanding for a long time. Industry 
benefits from the practice for the same reasons but also because many 
programmers understand a lot of the overall system. If a programmer moves 
on, those who stay can easily reconstruct the workings of any component.

Read more about pair programming in William and Kessler's well-known CACM 
article on pair programming and William's dissertation on the same topic, 
which has appeared as a jointly authored book (Pair Programming Illuminated) 
with Addison and Wesley. 
