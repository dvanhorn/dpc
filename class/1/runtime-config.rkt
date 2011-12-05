(module runtime-config racket/base

  (provide configure)
  (require class1/lang/reader)

  (define (configure config)
    (current-readtable rt)))

