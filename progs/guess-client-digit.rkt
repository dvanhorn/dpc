#lang class/1
(provide no-guess%)
(require class/universe)
(require 2htdp/image)

;; A Client is one of:
;; - Waiting
;; - Accepting
;;
;; A Waiting implements:
;; - to-draw : -> Scene
;; - on-receive : SExp -> Client
;;
;; An Accepting implements:
;; - to-draw : -> Scene
;; - on-key : SExp -> Client

(define MT-SCENE (empty-scene 600 200))
;; String -> Image
(define (txt str)
  (text str 40 'red))

;; A (new no-guess%) implements Accepting
(define-class no-guess%
  (define (register) LOCALHOST) ; Change to play remotely

  (check-expect ((new no-guess%) . to-draw)
                (overlay (txt "Take a guess") MT-SCENE))
  (define (to-draw)
    (overlay (txt "Take a guess") MT-SCENE))

  (check-expect ((new no-guess%) . on-key "h")
                (new no-guess%))
  (check-expect ((new no-guess%) . on-key "7")
                (make-package (new waiting% 7) 7))
  (define (on-key ke)
    (local [(define n (string->number ke))]
      (cond [(number? n)
             (make-package (new waiting% n) n)]
            [else this]))))

;; A (new waiting% Number) implements Waiting
(define-class waiting%
  (fields n)

  (check-expect ((new waiting% 5) . to-draw)
                (overlay (txt "Guessed: 5") MT-SCENE))
  (define (to-draw)
    (overlay (beside (txt "Guessed: ")
                     (txt (number->string (this . n))))
             MT-SCENE))

  (check-expect ((new waiting% 5) . on-receive "too small")
                (new inform% 5 "too small"))
  (check-expect ((new waiting% 5) . on-receive 'something)
                (new waiting% 5))
  (define (on-receive msg)
    (cond [(string? msg)
           (new inform% (this . n) msg)]
          [else this])))


;; A (new inform% Number String) implements Accepting
(define-class inform%
  (fields n msg)

  (check-expect ((new inform% 7 "too small") . to-draw)
                (overlay (txt "Guessed: 7; too small") MT-SCENE))
  (define (to-draw)
    (overlay (beside (txt "Guessed: ")
                     (txt (number->string (this . n)))
                     (txt "; ")
                     (txt (this . msg)))
             MT-SCENE))

  (check-expect ((new inform% 7 "too small") . on-key "a")
                (new inform% 7 "too small"))
  (check-expect ((new inform% 7 "too small") . on-key "9")
                (make-package (new waiting% 9) 9))
  (define (on-key ke)
    (local [(define n (string->number ke))]
      (cond [(number? n)
             (make-package (new waiting% n) n)]
            [else this]))))

