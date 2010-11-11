#lang racket
(require "server.rkt"
         "client.rkt")
(require 2htdp/universe)
         
(provide (all-defined-out))

(define (play-solo n)
  (launch-many-worlds (serve n)
                      (play LOCALHOST (list 0 empty empty))))

(define (demo n)
  (launch-many-worlds (serve n)
                      (play LOCALHOST (list 0 empty empty))
                      (play LOCALHOST (list 0 empty empty))))

;(play-solo 75)
;(play "129.10.117.100" (list 0 (list) (list)))
;(play LOCALHOST (list 0 (list) (list)))
