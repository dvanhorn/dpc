(module runtime-config racket/base

  (provide configure)
  (require class/1/lang/reader)

  (define (configure config)
    (current-readtable rt)))

