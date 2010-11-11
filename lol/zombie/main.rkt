#lang racket
;; Zombie MAIN
;; Provides client and server functions, and some compositions.
(provide (all-defined-out)
         (all-from-out "server.rkt" 
                       "client.rkt"))
(require "server.rkt"
         "client.rkt")
(require 2htdp/universe)         

(define (play-solo n)
  (launch-many-worlds (serve n)
                      (play LOCALHOST)))

(define (demo n)
  (launch-many-worlds (serve n)
                      (play LOCALHOST)
                      (play LOCALHOST)))

;(play-solo 75)
;(play "129.10.117.100" (list 0 (list) (list)))
;(play LOCALHOST (list 0 (list) (list)))
