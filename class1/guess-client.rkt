#lang class1
(require class1/universe)
(require 2htdp/image)

(define-class guess-world%
  (fields status)
  
  (define/public (on-receive m)
    (new guess-world% m))
 
  
  (define/public (to-draw)
    (overlay (text (field status)
                   40
                   "red")
             (empty-scene 300 100)))
  
  (define/public (on-key k)
    (local [(define n (string->number k))]
      (if (number? n)
          (make-package this n)
          this)))                    
  
  (define/public (register) LOCALHOST))

(launch-many-worlds
 (big-bang (new guess-world% "guess a number"))
 (big-bang (new guess-world% "guess a number")))