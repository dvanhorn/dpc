#lang racket
(require "server.rkt"
         "client.rkt")
(require 2htdp/universe)
         
(provide (all-defined-out))

(define (play-solo n)
  (launch-many-worlds (play LOCALHOST (world 0 (set) (set)))
                      (serve n)))

(define (demo n)
  ;; Run program, run!
  (launch-many-worlds
   (play LOCALHOST (world 0 (set) (set)))
   (play LOCALHOST (world 0 (set) (set)))
   (serve n)))

;(play "129.10.117.100" (world 0 (set) (set)))
;(play LOCALHOST (world 0 (set) (set)))
