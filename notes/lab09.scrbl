#lang scribble/manual

@(require scribble/eval
          racket/sandbox
          "../web/lab.rkt"
          "../web/unnumbered.rkt"
          "../web/utils.rkt"
          (for-label (except-in class3 define-struct)
                     2htdp/image
                     (only-in lang/htdp-intermediate-lambda define-struct)
                     class1/universe
                     (only-in racket/string string-join)
                     (only-in racket/base   display)))

@(define the-eval
  (let ([the-eval (make-base-eval)])
    (the-eval '(require class3))
    ;(the-eval '(require 2htdp/image))
    ;(the-eval '(require class3/universe))
    the-eval))

@(define exercise (exercise-counter))

@title[#:tag "lab09"]{3/07: Scraping HTML with visitors}

@exercise{
  Install @url["class3-html.plt"]. It adds the @racket[class3/html] module that
  we'll be using in this lab to interact with html.
}

Consider the following program:

@#reader scribble/comment-reader
(racketmod
class3
(require class3/html)
(require (only-in racket/string string-join))
(require (only-in racket/base display))

; HtmlElement -> String
; Scrape text from html
(define (scrape-text html)
  (html #,dot visit (text-scraper%)))

; Visitor to scrape text from html
; Implements [HtmlVisitor String]
(define-class text-scraper%

  ; String -> String
  ; Leave text as it is
  (define/public (visit-text t)
    t)

  ; Attrs [Listof String] -> String
  ; For elements, concatenate the text scraped from their children
  (define/public (visit-element attrs lo-text)
    (string-join lo-text " ")))

(display (scrape-text (parse-html (fetch-url "http://racket-lang.org"))))
)

When you run it, it @racket[display]s the text from @tt{racket-lang.org} with
none of the html structure or formatting. It achives this not by decomposing the
html returned by @racket[parse-html], but by constructing an @tt{HtmlVisitor}:

@#reader scribble/comment-reader
(racketblock
; An [HtmlVisitor X] implements:
;
;   visit-text : String -> X
;   Produce an X from text
;
;   visit-element : Attrs [Listof X] -> X
;   Produce an X from an element's attributes and the Xs from its children
;
; and for any html tag <tag>, it may also implement:
;
;   visit-<tag> : Attrs [Listof X] -> X
;   Produce an X from a <tag>'s attributes and the Xs made by its children.
;   If visit-<tag> exists, call it instead of visit-element for <tag> elements.
)

In fact, all it knows about @tt{HtmlElement}s is that each has a @racket[visit]
method:

@#reader scribble/comment-reader
(racketblock
; An HtmlElement implements:
;
;   visit : [HtmlVisitor X] -> X
;   Produce an X from this element using the given visitor
)

and that html attributes support a dictionary-like lookup interface:

@#reader scribble/comment-reader
(racketblock
; An Attrs implements:
;
;   get : String -> (or String false)
;   Get the value of the given attribute, or false if it doesn't exist
)

@exercise{
  In html, links are encoded as @tt{a} elements with an @tt{href} attribute:

  @indented{
    @tt{Check out <a href="http://racket-lang.org">this sweet language</a>!}
  }

  Write a visitor @racket[link-scraper%] and a function @racket[scrape-links]
  @tt{: HtmlElement -> [Listof URL]}, where @tt{URL = String}, that scrapes the
  urls of the links on a page.

  To do this, @racket[link-scraper%] needs the two required methods
  @racket[visit-text] and @racket[visit-element], but you should also give it
  the optional method @racket[visit-a] to specialize its behavior on @tt{a}
  elements.

  Your @racket[link-scraper%] visitor should implement @tt{[HtmlElement X]} for
  some @tt{X}---what would be a good choice of @tt{X}?
}

@exercise{
  Images are encoded as @tt{img} elements with a @tt{src} attribute:

  @indented{
    @tt{Here is a cool google doodle: <img
    src="http://gmsearchscripts.googlecode.com/files/lego.JPG"></img>}
  }

  Write a visitor @racket[image-scraper%] and a function @racket[scrape-images]
  @tt{: HtmlElement -> [Listof URL]} that scrapes the urls of the images on a
  page.

  Again, what would be a good choice of @tt{X}?
}

If you want to find out how something is encoded in html, most browsers have a
@emph{View Source} command, typically in the right-click context menu. With this
you can inspect the html of anything you see on the web.

@exercise{
  Javascript code (which is not actually related to java in any meaningful
  sense) is included into html in various ways, one of which is using
  @tt{script} elements with a @tt{type="text/javascript"} attribute and a
  @tt{src} attribute specifying the location of the code:

  @indented{
    @tt{<script type="text/javascript" src="/js/jquery-1.4.4.js"></script>}
  }

  Write a visitor @racket[js-scraper%] and a function @racket[scrape-js] @tt{:
  HtmlElement -> [Listof URL]} that scrapes the urls of javascript files loaded
  into a page.
}

@exercise{
  One of java's early hits was applets: you compile some java gui code, make the
  @tt{.class} file visible on the web, link to it with an @tt{applet} tag, and
  then your page includes a little pane running a graphical java program.

  But applets are so 90's! Write a visitor @racket[applet-detector%] and a
  function @racket[applets?] @tt{: HtmlElement -> Boolean} that inspects a page
  to determine whether it includes any applets.

  What would be good choice for @tt{X}?
}

@exercise{
  When you click a link with someone's email address, it's similar to a normal
  link except the email address is made into a url by adding the @tt{"mailto:"}
  prefix:

  @indented{
    @tt{Send all complaints to <a href="mailto:dvanhorn@"@"ccs.neu.edu">this
    guy</a>.}
  }

  Write a visitor @racket[email-scraper%] and function @racket[scrape-emails]
  that scrapes all the email addresses from a page. (Step 2: Sell them to
  spammers. Step 3: Mourn for your cold, black heart.)
}

@exercise{
  When browsing around for research papers, I often encounter pages that include
  a bunch of .pdf files, and I want to batch-download all of them. Or maybe it's
  a bunch of .png charts. Or maybe some .mp3 audio.

  Write a visitor @racket[file-ext-scraper%], and a function
  @racket[scrape-file-ext] @tt{ : String HtmlElement -> [Listof URL]} that takes
  a file extension (like @racket["pdf"] or @racket["png"]) and some html and
  finds all the urls that link to files with the given file extension.
}

@exercise{
  The title you see in the titlebar of a web page is specified by a @tt{title}
  tag:

  @indented{
    @tt{<title>My first geocities home page (under construction!)</title>}
  }

  Write a scraper that extracts the title text from a page. You may either
  assume that there is only one @tt{title} element, or simply concatenate the
  text from multiple @tt{title}s that you find.

  ...This is a little trickier than it sounds. When you're in the
  @racket[visit-title] method, you can't simply ask for its text, and when
  you're in @racket[visit-text], you can't simply ask whether the text belongs
  to a @tt{title} element.

  And the answer isn't @racket[set-field!]. You can do it with only what you
  learned in Fundies 1.
}

@exercise{
  Another way to add javascript code to the web is to write it inline in your
  html:

  @indented{@tt{
    <script type="text/javascript"> @linebreak{}
    alert("Welome to my awesome geocities home page!"); @linebreak{}
    </script>
  }}

  Write a scraper that extracts all @emph{inline} javascript code from a page.
}

@exercise{
  As you move from page to page, the web has various ways to remember things
  about you, like what you put in your shopping cart or the zipcode you typed in
  for food delivery. One of them is for the server to include ``hidden inputs''
  in the html it gives you. For example, if I enter my zipcode into a form and
  the server sends me to a new page, it could remember my zipcode by including
  in the new page as:

  @indented{
    @tt{<input type="hidden" name="zipcode" value="02446"></input>}
  }

  Then, when I submit another form from the next page, the information
  @tt{zipcode="02446"} will be sent along with it.

  I'm always curious what information is being piped around. Write a scraper
  that finds all the @tt{name}-@tt{value} pairs for each hidden input in a page.
}

@exercise{
  The web has many, many ways of making text bold, underlined, red, small, etc.
  One common way to emphasize text is with the @tt{b} or @tt{strong} tags for
  bold (they have the same behavior), and with the @tt{i} or @tt{em} tags for
  italics.

  Write a scraper that extracts all important text---text in bold or
  italics---from a page.
}

@exercise{
  Page headings are encoded with one of the tags @tt{h1}, @tt{h2}, @tt{h3},
  @tt{h4}, @tt{h5}, or @tt{h6}, depending on how deeply the heading is nested
  beneath other headings. (So an @tt{h4} would be a sub-sub-sub-heading.)

  Write a scraper that extracts the list of headings from a page. This is very a
  crude outline of the content on the page, as structured by the headings (if
  any).
}

@exercise{@bold{(Challenge)}
  Write a scraper that extracts a better outline. Extract the headings as
  before, but present them not just as a flat list but as some data of your own
  design that preserves the nesting structure.
}
