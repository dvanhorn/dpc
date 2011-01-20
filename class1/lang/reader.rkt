#lang s-exp syntax/module-reader
class0
#:read 
(λ args
  (parameterize ([read-decimal-as-inexact #f]
                 [read-accept-dot #f]
                 [read-accept-quasiquote #t])
    (apply read args)))

#:read-syntax 
(λ args
  (parameterize ([read-decimal-as-inexact #f]
                 [read-accept-dot #f]
                 [read-accept-quasiquote #t])
    (apply read-syntax args)))

#:info (make-info '(abbreviate-cons-as-list read-accept-quasiquote))
#| #:language-info (make-language-info '(abbreviate-cons-as-list read-accept-quasiquote)) |#
                 
(define ((make-info options) key default use-default)
  (case key
    [(drscheme:toolbar-buttons)
     (list (dynamic-require 'stepper/drracket-button 'stepper-drracket-button)
           (dynamic-require 'drracket/syncheck-drracket-button 'syncheck-drracket-button))]

    [(drscheme:opt-out-toolbar-buttons)
     ;; opt-out of all of the extra buttons b/c 
     ;; we don't want anything to confuse in the teaching languages.
     #f]
    
    [else (use-default key default)]))

(define (make-language-info options)
  `#(htdp/bsl/language-info get-info ,options))
  
;(require htdp/isl/lang/reader)
