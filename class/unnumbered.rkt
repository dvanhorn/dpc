#lang racket

(require scribble/manual)
(require scribble/core)
(require (for-syntax syntax/parse))
(require "keywords.rkt")

(provide (all-defined-out))

(define/contract (normalize-style x)
  (-> (or/c style? #f string? symbol? (listof symbol?)) style?)
  (match x
    [(? style? s)              s]
    [#f                        (style #f null)]
    [(? string? n)             (style n null)]
    [(? symbol? p)             (style #f (list p))]
    [(list (? symbol? ps) ...) (style #f ps)]))

(define (style-add-props s . props)
  (style (style-name s)
         (append (style-properties s) props)))

(define (kw-style-add-props kws . props)
  (dict-set kws
            '#:style
            (apply style-add-props
                   (normalize-style (dict-ref kws '#:style #f))
                   props)))

(define-for-syntax (prefix-syntax cxt pre stx)
  (datum->syntax cxt (string->symbol
                       (string-append (symbol->string pre)
                                      (symbol->string (syntax->datum stx))))))

(define (unnumbered f)
  (λ/kw-dict (kws . args)
    (keyword-apply/dict f (kw-style-add-props kws 'unnumbered) args)))

(define title*         (unnumbered title))
(define section*       (unnumbered section))
(define subsection*    (unnumbered subsection))
(define subsubsection* (unnumbered subsubsection))
