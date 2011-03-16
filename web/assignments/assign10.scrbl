#lang scribble/manual
@(require "../utils.rkt"
	  "../unnumbered.rkt"
          (for-label class1))

@title[#:tag "assign07"]{3/25: Stable Marriage}

Due: 3/25 (Non-standard day).

Daisy, Daisy, give me your answer, do. 

I'm half crazy, all for the love of you. 

It won't be a stylish marriage! I can't afford a carriage, 

But you'll look sweet, upon the seat of a bicycle built for two.

Marriage as a societal institution has been with us for many years,
though in times of social flux or upheaval the stability of the
institution is often subject to question, if not shaken outright. God
made light, heavens and earth; He filled the seas with great whales;
He created Man and Woman and commanded them to "be fruitful and
multiply." First there was Adam and Eve... sometime later Abraham and
Sarah, Isaac and Rebekah, and a long time afterwards Henry VIII and
his succession of marriages, Elizabeth Taylor and Richard Burton
(twice! or was it three times...), and so on to the current
day. Without question, love is as powerful an emotion as it was in the
good old days, but how love is manifested in marriage is as varied as
the number of marriages that have taken place.

It is generally not recognized that, despite the emotive aspects of
love and romance, marriage is fun- damentally an engineering
problem. As such, it is amenable to a careful mathematical and
computational analysis, emplying principles of programming and finite
combinatorics. In this problem set, we will use the paradigm of
message-passing programming, combined with the idea of encoding state
information in procedures, to provide, for the first time in human
history, @emph{a definitive solution to the problem of stability in
marriage}.

Let's assume, for the sake of an argument, that you are a village
matchmaker given the task of marrying 100 men and 100 women. Each of
the men has ranked the women from 1 to 100 in the order of his
preference; each woman, not to be outdone, has similarly ranked the
100 men. As a matchmaker, it is clear that in producing 100 couples,
you will not be able to give each person his first choice: for
example, if Alan is the first choice of all the women, only one will
get him, and all other women will be left with their second (or worse)
choices.

Rather than insist that everyone get their first choice, you are
instead charged with creating 100 stable marriages. A set of marriages
is said to be stable if there exists no man @emph{m} and woman
@emph{w} such that @emph{m} likes @emph{w} better than his wife, and
@emph{w} likes @emph{m} better than her husband. The notion is called
"stability" as @emph{m}'s and @emph{w}'s marriages are unstable, since
(albeit sordid and tawdry) @emph{m} and @emph{w} could optimize their
sorry lot in life by leaving their mates and running off together.

@bold{The algorithm}

We first describe in English an algorithm to find stable
marriages. Each person has a preference list of members of the
opposite sex. The first time a person proposes, it is to their number
one choice, the second time to their number two choice, and so on. We
will assume, without loss of non-sexist generality, that men always
propose to women (although there is no reason that it couldn't happen
the other way around), and that the proposals occur in
@emph{rounds}. At any moment in time, each person is either
@emph{engaged} or @emph{unengaged}, and of course initially everyone
is unengaged. (Assuming heterosexual alliances only, we can
further---in the interest of mathematical simplicity---assert that the
number of engaged men equals precisely the number of engaged women.)


At the start of a round, each @emph{unengaged} man is ordered by you,
the matchmaker, to propose to the woman he loves most but as yet to
whom he has not proposed. Each woman responds according to her status
as given in the following case analysis:

@itemlist[ 

@item{@bold{Unengaged, received no proposals:} Sit tight, life is
bound to improve. Your prince will be arriving shortly.}  

@item{@bold{Unengaged, received one proposal:} Accept the proposal
(the man and woman are now engaged).}

@item{@bold{Unengaged, received more than one proposal:} Accept the
proposal you like the best based on @emph{your} preference list, telling the
other suitors to buzz off (Wonder Man and woman now engaged).}

@item{@bold{Engaged, received no proposals:} Declare your eternal and
undying love to your sweetie.}

@item{@bold{Engaged, received one proposal:} If you like the man who
proposed more than your current main squeeze, dump your fiance and
reengage yourself to this more inviting gentleman. You and the
proposer are now engaged; Mr. Dumped is now unengaged. Otherwise, say
"I'm flattered but I'm not interested," and declare your undying love
for your intended.}

@item{@bold{Engaged, received more than one proposal:} Choose the
proposal you like best by looking at your preference list, and act as
if you are currently engaged and this gentleman alone has proposed.}

]

At the end of a round and the ensuing musical chairs, some men are
engaged and others are not. If everyone is engaged, stop: the current
pairs are the marriages that the matchmaker seeks. Otherwise, do
another round.

Of course, no assurances have been made that the marriages produced
are stable, or even that everyone is engaged at the end. For example,
is it possible that a man proposes 100 times, exhausting his proposal
list, and gets rejected every time? We’ll address these questions
later on; for the moment, let’s assume that the method works
correctly.

@bold{An Object Representation of People}

Do you really believe that people are just computers, brain meat is
nothing but a central processing unit, and the thoughts, hopes, and
dreams that keep our life alive are nothing but software being
executed? Of course not. (However, I'm sad to say, there are computer
scientists and cognitive psychologists who do.) Nonetheless, for the
purposes of this problem set, we will model certain salient features
of people by objects:

Here is the interface, and some examples, for a person.

@verbatim{
A Person implements:

name : -> String
The name of this person

intended : -> Person or #f
This person's intended significant other, if any.

preferences : -> [Listof Person]
People this person would marry, in order of preference.

possibles : -> [Listof Person]
People this person has not proposed to, but is still interested in, in
order of preference.

load-preferences! : [Listof Person] -> 
Effect: set this person's prefences to the given list.

reset! : -> 
Effect: Reinitialize this person's intended (#f), possibles (empty), and
preferences (empty).

propose! : -> Boolean
Effect: Propose to the most desired person not already proposed to.
Produces true if proposal is accepted, false if rejected.

i-love-you! : Person -> Boolean
This message represents a proposal from the given person to this 
person.
Effect: set the intended to the given person, if accepted.
Produce true if accepted, false otherwise.

i-changed-my-mind! : Person -> 
This message represents a break up from the given person to this
person.
Effect: set the intended to #f.
Assume: this person is the given person's intended and vice versa.

i-like-more? : Person Person -> Boolean
Does this person like the first person more than the second?

couple? : Person -> Boolean
Are this person and the given person engaged to each other?
}

@itemlist[#:style 'ordered
 @item{@bold{People.}
 Design a class that implements the @tt{Person} interface.}

 @item{@bold{Match making.}
 Design the following function:

@verbatim{
courtship! : [Listof People] [Listof People] -> [Listof (list String String)]
Effect: Marry all of the given proposers and proposees, producing
a stable configuration.
Produces a list of couple's name, proposer first.
}

The @tt{courtship!} function should be an implementation of the stable
marriage algorithm described above.

Implement any additional functions or classes needed to implement the
stable marriage algorithm.
}
	
 @item{@bold{Java.}
  Re-write your stable marriage program in Java.}
]