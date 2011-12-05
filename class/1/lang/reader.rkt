#lang s-exp syntax/module-reader
class1

#:language-info '#(class1/language-info get-info #f)

#:read 
(λ args
  (parameterize ([read-decimal-as-inexact #f]
                 [read-accept-dot #f]
                 [read-accept-quasiquote #t]
                 [current-readtable rt])
    (apply read args)))

#:read-syntax 
(λ args
  (parameterize ([read-decimal-as-inexact #f]
                 [read-accept-dot #f]
                 [read-accept-quasiquote #t]
                 [current-readtable rt])
    (apply read-syntax args)))

#|#:info (make-info '(abbreviate-cons-as-list read-accept-quasiquote))|#
#| #:language-info (make-language-info '(abbreviate-cons-as-list read-accept-quasiquote)) |#
                 
(define rt (make-readtable #f
                           #\. 'non-terminating-macro 
			   (lambda (char port . xs) 
			     (if (char-whitespace? (peek-char port))
				 '|.|
				 (read/recursive port #\. #f)))))



(provide rt)  
;(require htdp/isl/lang/reader)
