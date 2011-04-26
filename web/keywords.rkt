#lang racket

(require srfi/1
         #;
         (only-in unstable/text text<?))

(provide (all-defined-out))

(define (text->string v)
  (if (keyword? v)
      (keyword->string v)
      v))

(define (text<? one two) (string<? (text->string one) (text->string two)))

;
; Using keywords
;

(define (make-keyword-procedure/dict f)
  (make-keyword-procedure
    (λ (kw-ids kw-args . rest)
      (apply f (make-immutable-hash (map cons kw-ids kw-args)) rest))))

(define (keyword-apply/dict f kws . rest)
  (let-values ([(kw-ids kw-args)
                (apply values
                       (unzip (sort (dict-map kws list) #:key car text<?)))])
    (keyword-apply f kw-ids kw-args (apply list* rest))))

(define-syntaxes (λ/kw-dict lambda/kw-dict)
  (let ([t (syntax-rules ()
             [(_ args body ...)
              (make-keyword-procedure/dict (lambda args body ...))])])
    (values t t)))

(define-syntax define/kw-dict
  (syntax-rules ()
    [(_ (f x ...) e ...)      (define/kw-dict f (λ/kw-dict (x ...) e ...))]
    [(_ (f x ... . xs) e ...) (define/kw-dict f (λ/kw-dict (x ... . xs) e ...))]
    [(_ f e ...)              (define f e ...)]))

;
; Dictionaries
;

; dict-extend : dict ... -> dict
(define (dict-extend . ds)
  ((apply compose
          (reverse (concat (map (curryr dict-map (curry curryr dict-set)) ds))))
   '()))

; dict-remove* : dict key ... -> dict
(define (dict-remove* d . ks)
  (foldr (λ (k d) (dict-remove d k)) d ks))

;
; Lists
;

; concat : (Listof (Listof a)) -> (Listof a)
(define (concat ls)
  (apply append ls))

; unzip : (Listof (List a ...)) -> (List (Listof a) ...)
(define (unzip l)
  (apply zip l)) ; transpose
