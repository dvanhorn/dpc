#lang class3
(require "class3/html.rkt")
(require (only-in racket/string string-join))
(require (only-in racket/pretty pretty-display))

; An [HtmlVisitor X] implements:
;
;   visit-text : String -> X
;   What to do with text
;
;   visit-element : Attrs [Listof X] -> X
;   What to do with elements in general
;
; and for any html tag <tag>, it may also implement:
;
;   visit-<tag> : Attrs [Listof X] -> X
;   What to do with <tag> elements in particular

; An HtmlElement implements:
;
;   visit : [HtmlVisitor X] -> X
;   Traverse using the given visitor

; An Attrs implements
;
;   get : String -> (or String false)
;   Lookup the value of the given attribute, or false if it doesn't exist.

; An HtmlElement is one of:
;   (a%          Attrs [Listof HtmlElement])
;   (abbr%       Attrs [Listof HtmlElement])
;   (acronym%    Attrs [Listof HtmlElement])
;   (address%    Attrs [Listof HtmlElement])
;   (applet%     Attrs [Listof HtmlElement])
;   (area%       Attrs [Listof HtmlElement])
;   (b%          Attrs [Listof HtmlElement])
;   (base%       Attrs [Listof HtmlElement])
;   (basefont%   Attrs [Listof HtmlElement])
;   (bdo%        Attrs [Listof HtmlElement])
;   (big%        Attrs [Listof HtmlElement])
;   (blockquote% Attrs [Listof HtmlElement])
;   (body%       Attrs [Listof HtmlElement])
;   (br%         Attrs [Listof HtmlElement])
;   (button%     Attrs [Listof HtmlElement])
;   (caption%    Attrs [Listof HtmlElement])
;   (center%     Attrs [Listof HtmlElement])
;   (cite%       Attrs [Listof HtmlElement])
;   (code%       Attrs [Listof HtmlElement])
;   (col%        Attrs [Listof HtmlElement])
;   (colgroup%   Attrs [Listof HtmlElement])
;   (dd%         Attrs [Listof HtmlElement])
;   (del%        Attrs [Listof HtmlElement])
;   (dfn%        Attrs [Listof HtmlElement])
;   (dir%        Attrs [Listof HtmlElement])
;   (div%        Attrs [Listof HtmlElement])
;   (dl%         Attrs [Listof HtmlElement])
;   (dt%         Attrs [Listof HtmlElement])
;   (em%         Attrs [Listof HtmlElement])
;   (fieldset%   Attrs [Listof HtmlElement])
;   (font%       Attrs [Listof HtmlElement])
;   (form%       Attrs [Listof HtmlElement])
;   (h1%         Attrs [Listof HtmlElement])
;   (h2%         Attrs [Listof HtmlElement])
;   (h3%         Attrs [Listof HtmlElement])
;   (h4%         Attrs [Listof HtmlElement])
;   (h5%         Attrs [Listof HtmlElement])
;   (h6%         Attrs [Listof HtmlElement])
;   (head%       Attrs [Listof HtmlElement])
;   (hr%         Attrs [Listof HtmlElement])
;   (html%       Attrs [Listof HtmlElement])
;   (i%          Attrs [Listof HtmlElement])
;   (iframe%     Attrs [Listof HtmlElement])
;   (img%        Attrs [Listof HtmlElement])
;   (input%      Attrs [Listof HtmlElement])
;   (ins%        Attrs [Listof HtmlElement])
;   (isindex%    Attrs [Listof HtmlElement])
;   (kbd%        Attrs [Listof HtmlElement])
;   (label%      Attrs [Listof HtmlElement])
;   (legend%     Attrs [Listof HtmlElement])
;   (li%         Attrs [Listof HtmlElement])
;   (link%       Attrs [Listof HtmlElement])
;   (menu%       Attrs [Listof HtmlElement])
;   (meta%       Attrs [Listof HtmlElement])
;   (noframes%   Attrs [Listof HtmlElement])
;   (noscript%   Attrs [Listof HtmlElement])
;   (object%     Attrs [Listof HtmlElement])
;   (ol%         Attrs [Listof HtmlElement])
;   (optgroup%   Attrs [Listof HtmlElement])
;   (option%     Attrs [Listof HtmlElement])
;   (p%          Attrs [Listof HtmlElement])
;   (param%      Attrs [Listof HtmlElement])
;   (pre%        Attrs [Listof HtmlElement])
;   (q%          Attrs [Listof HtmlElement])
;   (s%          Attrs [Listof HtmlElement])
;   (samp%       Attrs [Listof HtmlElement])
;   (script%     Attrs [Listof HtmlElement])
;   (select%     Attrs [Listof HtmlElement])
;   (small%      Attrs [Listof HtmlElement])
;   (span%       Attrs [Listof HtmlElement])
;   (strike%     Attrs [Listof HtmlElement])
;   (strong%     Attrs [Listof HtmlElement])
;   (style%      Attrs [Listof HtmlElement])
;   (sub%        Attrs [Listof HtmlElement])
;   (sup%        Attrs [Listof HtmlElement])
;   (table%      Attrs [Listof HtmlElement])
;   (tbody%      Attrs [Listof HtmlElement])
;   (td%         Attrs [Listof HtmlElement])
;   (textarea%   Attrs [Listof HtmlElement])
;   (tfoot%      Attrs [Listof HtmlElement])
;   (th%         Attrs [Listof HtmlElement])
;   (thead%      Attrs [Listof HtmlElement])
;   (title%      Attrs [Listof HtmlElement])
;   (tr%         Attrs [Listof HtmlElement])
;   (tt%         Attrs [Listof HtmlElement])
;   (u%          Attrs [Listof HtmlElement])
;   (ul%         Attrs [Listof HtmlElement])
;   (var%        Attrs [Listof HtmlElement])

; Scrape string of all text on page
; X = String
(define-class text-scraper%
  (define/public (visit-text t)
    t)
  (define/public (visit-element attrs lo-text)
    (string-join lo-text " ")))

; Scrape list of all links on page
; X = (Listof URL)
(define-class link-scraper%
  (define/public (visit-text t)
    empty)
  (define/public (visit-element attrs lo-lo-urls)
    (apply append lo-lo-urls))
  (define/public (visit-a attrs lo-lo-urls)
    (apply append
           (if (equal? false (attrs . get "href"))
             (list (attrs . get "href"))
             (list))
           lo-lo-urls)))

; Scrape list of image urls
; X = (Listof URL)
(define-class image-scraper%
  (define/public (visit-text t)
    empty)
  (define/public (visit-element attrs lo-lo-urls)
    (apply append lo-lo-urls))
  (define/public (visit-img attrs lo-lo-urls)
    (apply append
           (if (equal? false (attrs . get "src"))
             (list (attrs . get "src"))
             (list))
           lo-lo-urls)))

; Scrape list of all javascript links on page
; X = (Listof URL)
(define-class js-link-scraper%
  (define/public (visit-text t)
    empty)
  (define/public (visit-script attrs no-children)
    (if (and (equal? (attrs . get "type") "text/javascript")
             (equal? false (attrs . get "src")))
      (list (attrs . get "src"))
      empty))
  (define/public (visit-element attrs lo-lo-js)
    (apply append lo-lo-js)))

; Detect whether applets are used
; X = Boolean
(define-class applet-detector%
  (define/public (visit-text t)
    false)
  (define/public (visit-element attrs lo-applets?)
    (ormap (λ (x) x) lo-applets?))
  (define/public (visit-applet attrs lo-applets?)
    true))

; Scrape list of linked-to email addresses
; X = (Listof String), helper
(define (starts-with s start)
  (equal? start (substring s 0 (length start))))
(define-class email-scraper%
  (define/public (visit-text t)
    empty)
  (define/public (visit-element attrs lo-lo-emails)
    (apply append lo-lo-emails))
  (define/public (visit-a attrs lo-lo-emails)
    (apply append
           (if (and (equal? false (attrs . get "href"))
                    (starts-with (attrs . get "href")
                                 "email:"))
             (list (substring (attrs . get "href") (length "email:")))
             (list))
           lo-lo-emails)))

; Scrape list of links with file extension given in constructor
; X = (Listof URL), helper, fields
(define (ends-with s end)
  (equal? end (substring s (- (length s) (length end)))))
(define-class file-extension-scraper%
  (fields ext)
  (define/public (visit-text t)
    empty)
  (define/public (visit-element attrs lo-lo-urls)
    (apply append lo-lo-urls))
  (define/public (visit-a attrs lo-lo-urls)
    (apply append
           (if (and (equal? false (attrs . get "href"))
                    (ends-with (attrs . get "href")
                               (string-append "." (field ext))))
             (list (attrs . get "href"))
             (list))
           lo-lo-urls)))

; Scrape title
; X = Boolean -> String
(define-class title-scraper%
  (define/public (visit-text t)
    (λ (b) (if b t "")))
  (define/public (visit-element attrs fs)
    (λ (b) (apply string-append (map (λ (f) (f b)) fs))))
  (define/public (visit-title attrs fs)
    (λ (b) (apply string-append (map (λ (f) (f true)) fs)))))

; Scrape text of all inline javascript on page
; X = Boolean -> String
(define-class js-inline-text-scraper%
  (define/public (visit-text t)
    (λ (b) (if b t "")))
  (define/public (visit-script attrs fs)
    (if (equal? (attrs . get "type") "text/javascript")
      (λ (b) (apply string-append (map (λ (f) (f true)) fs)))
      (λ (b) "")))
  (define/public (visit-element attrs fs)
    (λ (b) (apply string-append (map (λ (f) (f b)) fs)))))

; Scrape list of hidden inputs
; X = (Listof Input), Input = (make-input String String)
(define-struct input (name value))
(define-class hidden-input-scraper%
  (define/public (visit-text t)
    empty)
  (define/public (visit-element attrs lo-lo-inputs)
    (apply append lo-lo-inputs))
  (define/public (visit-input attrs lo-lo-inputs)
    (if (equal? "hidden" (attrs . get "type"))
      (list (make-input (attrs . get "name") (attrs . get "value")))
      (list))))

; Scrape important text
; X = Boolean -> String, copy × 4
(define-class important-text-scraper%
  (define/public (visit-text t)
    (λ (b) (if b t "")))
  (define/public (visit-element attrs fs)
    (λ (b) (apply append (map (λ (f) (f b)) fs))))
  (define/public (visit-b attrs fs)
    (λ (b) (apply append (map (λ (f) (f true)) fs))))
  (define/public (visit-strong attrs fs)
    (λ (b) (apply append (map (λ (f) (f true)) fs))))
  (define/public (visit-i attrs fs)
    (λ (b) (apply append (map (λ (f) (f true)) fs))))
  (define/public (visit-em attrs fs)
    (λ (b) (apply append (map (λ (f) (f true)) fs)))))

; Scrape list of headings
; X = Boolean -> (Listof String), copy × 6
(define-class heading-scraper%
  (define/public (visit-text t)
    (λ (b) (list (if b t ""))))
  (define/public (visit-element attrs fs)
    (λ (b) (apply append (map (λ (f) (f b)) fs))))
  (define/public (visit-h1 attrs fs)
    (λ (b) (list (apply string-append (map (λ (f) (f true)) fs)))))
  (define/public (visit-h2 attrs fs)
    (λ (b) (list (apply string-append (map (λ (f) (f true)) fs)))))
  (define/public (visit-h3 attrs fs)
    (λ (b) (list (apply string-append (map (λ (f) (f true)) fs)))))
  (define/public (visit-h4 attrs fs)
    (λ (b) (list (apply string-append (map (λ (f) (f true)) fs)))))
  (define/public (visit-h5 attrs fs)
    (λ (b) (list (apply string-append (map (λ (f) (f true)) fs)))))
  (define/public (visit-h6 attrs fs)
    (λ (b) (list (apply string-append (map (λ (f) (f true)) fs))))))

; ——————————————————————————————————————————————————————————————————————————————

(define url
  ;"http://localhost"
  ;"http://localhost/notes/"
  "file:///Users/danb/src/plt-scheme/doc/index.html"
  )
(define h (html url))

;(pretty-display h)
;(pretty-display (h . visit (link-scraper%)))
(pretty-display (h . visit (text-scraper%)))
;(pretty-display (h . visit (css-link-scraper%)))
;(pretty-display ((h . visit (css-inline-text-scraper%)) false))
