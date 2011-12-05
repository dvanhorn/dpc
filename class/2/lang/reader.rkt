#lang s-exp syntax/module-reader
class2

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

(require (only-in "../../class1/lang/reader.rkt" rt))
