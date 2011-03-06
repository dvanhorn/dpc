#lang class3
(require (only-in racket/base open-input-string format
                  provide all-defined-out))
(require racket/dict unstable/dict)
(require (only-in racket/class method-in-interface? object-interface))
(require (only-in racket/port port->string))
(require (except-in html html))
(require xml net/url)
(provide (all-defined-out))

; An Attr  is a  (list String String)
; An Attrs is an (attrs% (Listof Attr))
(define-class attrs%
  (fields dict)

  ; String -> Boolean
  (define/public (has attr)
    (dict-has-key? (field dict) attr))

  ; String -> String
  ; Assumes attr is present
  (define/public (get attr)
    (first (dict-ref/default (field dict) attr #f)))

  ; String String -> Attrs
  (define/public (set attr x)
    (dict-set (field dict) attr (list x))))

(define-class text%
  (fields string)
  (define/public (visit v)
    (send v visit-text (field string))))

(define-class html-element%
  (fields attrs children))

; FIXME Works, but the `define-class' is stuck in some other scope or phase
;(define-syntax define-html-element
;  (syntax-parser
;    [(_ tag)
;     (with-syntax ([tag%      (text->identifier #'tag '%)]
;                   [visit-tag (text->identifier 'visit- #'tag)])
;       #'(define-class foo%
;           (super html-element%)
;           (define/public (visit v)
;             (if (method-in-interface? 'visit-tag (object-interface v))
;               (send v visit-tag
;                       (field attrs)
;                       (map (λ (x) (send x visit v)) (field children)))
;               (send v visit-element
;                       (field attrs)
;                       (map (λ (x) (send x visit v)) (field children)))))))]))

; WARNING: Copy/paste ahead! ---------------------------------------------------

(define-class html%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-html (object-interface v))
      (send v visit-html
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class div%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-div (object-interface v))
      (send v visit-div
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class center%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-center (object-interface v))
      (send v visit-center
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class blockquote%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-blockquote (object-interface v))
      (send v visit-blockquote
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class ins%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-ins (object-interface v))
      (send v visit-ins
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class del%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-del (object-interface v))
      (send v visit-del
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class dd%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-dd (object-interface v))
      (send v visit-dd
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class li%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-li (object-interface v))
      (send v visit-li
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class th%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-th (object-interface v))
      (send v visit-th
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class td%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-td (object-interface v))
      (send v visit-td
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class iframe%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-iframe (object-interface v))
      (send v visit-iframe
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class noframes%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-noframes (object-interface v))
      (send v visit-noframes
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class noscript%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-noscript (object-interface v))
      (send v visit-noscript
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class style%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-style (object-interface v))
      (send v visit-style
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class script%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-script (object-interface v))
      (send v visit-script
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class basefont%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-basefont (object-interface v))
      (send v visit-basefont
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class br%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-br (object-interface v))
      (send v visit-br
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class area%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-area (object-interface v))
      (send v visit-area
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class link%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-link (object-interface v))
      (send v visit-link
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class img%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-img (object-interface v))
      (send v visit-img
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class param%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-param (object-interface v))
      (send v visit-param
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class hr%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-hr (object-interface v))
      (send v visit-hr
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class input%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-input (object-interface v))
      (send v visit-input
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class col%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-col (object-interface v))
      (send v visit-col
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class isindex%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-isindex (object-interface v))
      (send v visit-isindex
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class base%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-base (object-interface v))
      (send v visit-base
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class meta%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-meta (object-interface v))
      (send v visit-meta
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class option%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-option (object-interface v))
      (send v visit-option
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class textarea%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-textarea (object-interface v))
      (send v visit-textarea
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class title%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-title (object-interface v))
      (send v visit-title
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class head%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-head (object-interface v))
      (send v visit-head
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class tr%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-tr (object-interface v))
      (send v visit-tr
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class colgroup%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-colgroup (object-interface v))
      (send v visit-colgroup
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class thead%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-thead (object-interface v))
      (send v visit-thead
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class tfoot%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-tfoot (object-interface v))
      (send v visit-tfoot
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class tbody%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-tbody (object-interface v))
      (send v visit-tbody
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class tt%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-tt (object-interface v))
      (send v visit-tt
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class i%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-i (object-interface v))
      (send v visit-i
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class b%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-b (object-interface v))
      (send v visit-b
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class u%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-u (object-interface v))
      (send v visit-u
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class s%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-s (object-interface v))
      (send v visit-s
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class strike%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-strike (object-interface v))
      (send v visit-strike
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class big%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-big (object-interface v))
      (send v visit-big
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class small%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-small (object-interface v))
      (send v visit-small
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class em%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-em (object-interface v))
      (send v visit-em
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class strong%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-strong (object-interface v))
      (send v visit-strong
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class dfn%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-dfn (object-interface v))
      (send v visit-dfn
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class code%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-code (object-interface v))
      (send v visit-code
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class samp%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-samp (object-interface v))
      (send v visit-samp
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class kbd%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-kbd (object-interface v))
      (send v visit-kbd
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class var%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-var (object-interface v))
      (send v visit-var
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class cite%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-cite (object-interface v))
      (send v visit-cite
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class abbr%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-abbr (object-interface v))
      (send v visit-abbr
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class acronym%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-acronym (object-interface v))
      (send v visit-acronym
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class sub%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-sub (object-interface v))
      (send v visit-sub
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class sup%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-sup (object-interface v))
      (send v visit-sup
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class span%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-span (object-interface v))
      (send v visit-span
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class bdo%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-bdo (object-interface v))
      (send v visit-bdo
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class font%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-font (object-interface v))
      (send v visit-font
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class p%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-p (object-interface v))
      (send v visit-p
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class h1%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-h1 (object-interface v))
      (send v visit-h1
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class h2%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-h2 (object-interface v))
      (send v visit-h2
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class h3%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-h3 (object-interface v))
      (send v visit-h3
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class h4%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-h4 (object-interface v))
      (send v visit-h4
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class h5%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-h5 (object-interface v))
      (send v visit-h5
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class h6%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-h6 (object-interface v))
      (send v visit-h6
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class q%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-q (object-interface v))
      (send v visit-q
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class dt%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-dt (object-interface v))
      (send v visit-dt
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class legend%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-legend (object-interface v))
      (send v visit-legend
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class caption%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-caption (object-interface v))
      (send v visit-caption
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class table%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-table (object-interface v))
      (send v visit-table
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class button%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-button (object-interface v))
      (send v visit-button
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class fieldset%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-fieldset (object-interface v))
      (send v visit-fieldset
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class optgroup%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-optgroup (object-interface v))
      (send v visit-optgroup
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class select%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-select (object-interface v))
      (send v visit-select
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class label%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-label (object-interface v))
      (send v visit-label
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class form%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-form (object-interface v))
      (send v visit-form
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class ol%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-ol (object-interface v))
      (send v visit-ol
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class ul%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-ul (object-interface v))
      (send v visit-ul
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class dir%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-dir (object-interface v))
      (send v visit-dir
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class menu%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-menu (object-interface v))
      (send v visit-menu
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class dl%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-dl (object-interface v))
      (send v visit-dl
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class pre%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-pre (object-interface v))
      (send v visit-pre
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class object%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-object (object-interface v))
      (send v visit-object
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class applet%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-applet (object-interface v))
      (send v visit-applet
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class map%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-map (object-interface v))
      (send v visit-map
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class a%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-a (object-interface v))
      (send v visit-a
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class address%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-address (object-interface v))
      (send v visit-address
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

(define-class body%
  (super html-element%)
  (define/public (visit v)
    (if (method-in-interface? 'visit-body (object-interface v))
      (send v visit-body
              (field attrs)
              (map (λ (x) (send x visit v)) (field children)))
      (send v visit-element
              (field attrs)
              (map (λ (x) (send x visit v)) (field children))))))

; End copy/paste ---------------------------------------------------------------

(define-class unknown%
  (super html-element%)
  (define/public (visit v)
    (send v visit-element
            (field attrs)
            (map (λ (x) (send x visit v)) (field children)))))

(define (attrs x)
  (html-element-attributes x))

(define (children x)
  (if (html-full? x)
    (html-full-content x)
    empty))

(define (attrs->classes x)
  (attrs% (map (λ (a) (list (symbol->string (attribute-name a))
                            (attribute-value a))) x)))

(define (format-entity e) ; (far from a complete list)
  (cond
    [(equal? e 'nbsp)   " "]
    [(equal? e 'rarr)   "→"]
    [(equal? e 'ndash)  "–"]
    [(equal? e 'mdash)  "—"]
    [(equal? e 'hellip) "..."]
    [else               (format "&~s;" e)]))

; FIXME `object-name' helps here, but `eval' was giving me trouble
(define (structs->classes x)
  (cond

; WARNING: Copy/paste ahead! ---------------------------------------------------

    [(html? x)       (html%       (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(div? x)        (div%        (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(center? x)     (center%     (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(blockquote? x) (blockquote% (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(ins? x)        (ins%        (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(del? x)        (del%        (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(dd? x)         (dd%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(li? x)         (li%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(th? x)         (th%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(td? x)         (td%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(iframe? x)     (iframe%     (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(noframes? x)   (noframes%   (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(noscript? x)   (noscript%   (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(style? x)      (style%      (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(script? x)     (script%     (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(basefont? x)   (basefont%   (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(br? x)         (br%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(area? x)       (area%       (attrs->classes (attrs x)) (map structs->classes (children x)))]
    ; Renamed to `link%', below
    ;[(alink? x)      (alink%      (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(img? x)        (img%        (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(param? x)      (param%      (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(hr? x)         (hr%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(input? x)      (input%      (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(col? x)        (col%        (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(isindex? x)    (isindex%    (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(base? x)       (base%       (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(meta? x)       (meta%       (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(option? x)     (option%     (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(textarea? x)   (textarea%   (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(title? x)      (title%      (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(head? x)       (head%       (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(tr? x)         (tr%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(colgroup? x)   (colgroup%   (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(thead? x)      (thead%      (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(tfoot? x)      (tfoot%      (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(tbody? x)      (tbody%      (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(tt? x)         (tt%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(i? x)          (i%          (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(b? x)          (b%          (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(u? x)          (u%          (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(s? x)          (s%          (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(strike? x)     (strike%     (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(big? x)        (big%        (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(small? x)      (small%      (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(em? x)         (em%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(strong? x)     (strong%     (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(dfn? x)        (dfn%        (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(code? x)       (code%       (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(samp? x)       (samp%       (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(kbd? x)        (kbd%        (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(var? x)        (var%        (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(cite? x)       (cite%       (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(abbr? x)       (abbr%       (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(acronym? x)    (acronym%    (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(sub? x)        (sub%        (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(sup? x)        (sup%        (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(span? x)       (span%       (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(bdo? x)        (bdo%        (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(font? x)       (font%       (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(p? x)          (p%          (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(h1? x)         (h1%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(h2? x)         (h2%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(h3? x)         (h3%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(h4? x)         (h4%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(h5? x)         (h5%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(h6? x)         (h6%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(q? x)          (q%          (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(dt? x)         (dt%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(legend? x)     (legend%     (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(caption? x)    (caption%    (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(table? x)      (table%      (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(button? x)     (button%     (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(fieldset? x)   (fieldset%   (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(optgroup? x)   (optgroup%   (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(select? x)     (select%     (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(label? x)      (label%      (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(form? x)       (form%       (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(ol? x)         (ol%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(ul? x)         (ul%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(dir? x)        (dir%        (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(menu? x)       (menu%       (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(dl? x)         (dl%         (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(pre? x)        (pre%        (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(object? x)     (object%     (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(applet? x)     (applet%     (attrs->classes (attrs x)) (map structs->classes (children x)))]
    ;[(map? x)        (map%       (attrs->classes  (attrs x)) (map structs->classes  (children x)))]
    [(a? x)          (a%          (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(address? x)    (address%    (attrs->classes (attrs x)) (map structs->classes (children x)))]
    [(body? x)       (body%       (attrs->classes (attrs x)) (map structs->classes (children x)))]

; End copy/paste ---------------------------------------------------------------

    [(alink? x)      (link%       (attrs->classes (attrs x)) (map structs->classes (children x)))]

    [(pcdata? x)     (text%       (pcdata-string x))]
    [(cdata?  x)     (text%       (cdata-string x))]
    [(entity? x)     (text%       (format-entity (entity-text x)))]

    [else            (unknown%    (attrs x) (map structs->classes (children x)))]))

; A URL is a String

; fetch-url : URL -> String
; Fetch the given url and return its contents as a String
(define (fetch-url url)
  (port->string (get-pure-port (string->url url))))

; parse-html : String -> Html
; Parse the given string into an Html object
(define (parse-html s)
  (structs->classes (read-xhtml (open-input-string s))))

; html : URL -> Html
; Fetch the given url and parse it into an Html object
(define (html url)
  (parse-html (fetch-url url)))
