(module language-info racket/base

  (provide get-info)
  
  (define (get-info data)
    (lambda (key default)
      (case key
        [(configure-runtime)
         '(#(class/1/runtime-config configure #f))]
        [else default]))))

