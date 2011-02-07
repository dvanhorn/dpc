#lang class1
(require class1/universe)
(require 2htdp/image)
(require (only-in racket begin printf))

(define-class world%
  (fields down-key)

  (define/public (on-key k)
    (cond
      [(and (key-event? (field down-key))
            (key=? k (field down-key)))
       this]
      [else ((new world% k) . show-key 'press k)]))

  (define/public (on-release k)
    ((new world% false) . show-key 'release k))

  (define/public (show-key s k)
    (begin
      (printf "~s\t~s\n" s k)
      this))

  (define/public (to-draw)
    (empty-scene 100 100))

  )

(big-bang (new world% false))
