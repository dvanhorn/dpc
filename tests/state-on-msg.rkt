#lang class/0
;; Tests the use of the state pattern where subsequent server
;; states add an on-msg event handler.
;; See https://github.com/dvanhorn/dpc/issues/21
(require class/universe)
(require 2htdp/image)

(define-class wait%
  (define (on-new iw)
    (new begin%)))

(define-class begin%
  (define (on-msg iw msg)
    (make-bundle this (list (make-mail iw msg)) empty)))

(define-class client%
  (define (register) LOCALHOST)
  (define (on-tick)
    (make-package this 'tick!))
  (define (on-receive msg)
    (error "Got here!"))
  (define (to-draw)
    (empty-scene 20 20 "yellow")))

(check-error 
 (launch-many-worlds
  (universe (new wait%))
  (big-bang (new client%)))
 "Got here!")