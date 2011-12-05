#lang class3
(provide gzip%)
(require file/gunzip file/gzip)
(require (only-in racket/base open-output-bytes open-input-bytes get-output-bytes 
                  string->bytes/utf-8
                  bytes->string/utf-8))

(define-class gzip%
  (define/public (zip string)
    (local [(define o (open-output-bytes))]
      (begin (gzip-through-ports (open-input-bytes (string->bytes/utf-8 string))
                                 o #f 0)
             (get-output-bytes o))))
  (define/public (unzip bytes)
    (local [(define o (open-output-bytes))]
      (begin (gunzip-through-ports (open-input-bytes bytes) o)
             (bytes->string/utf-8 (get-output-bytes o))))))

