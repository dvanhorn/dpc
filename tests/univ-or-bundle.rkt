#lang class/0
;; Tests that universes may produce new universe states,
;; not just bundles.
;; See https://github.com/dvanhorn/dpc/issues/12
(require class/universe)
(require 2htdp/image)

(define-class wait%
  (define (on-new iw)
    (make-bundle (new begin%) empty empty)))

(define-class begin%
  (define (on-msg iw msg)
    (cond [(eq? msg 'done) (error "Got here!")]
          [else this])))

(define-class client%
  (define (register) LOCALHOST)
  (define (on-tick)
    (make-package (new finish%) 'tick!))
  (define (to-draw)
    (empty-scene 20 20 "yellow")))

(define-class finish%
  (define (on-tick)
    (make-package this 'done))
  (define (to-draw)
    (empty-scene 20 20 "red")))

(check-error 
 (launch-many-worlds
  (universe (new wait%))
  (big-bang (new client%)))
 "Got here!")