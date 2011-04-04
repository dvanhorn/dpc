#lang class5
(require class5/universe)
(require 2htdp/image)
(require racket/set)
(require (only-in racket when printf))

; 1. Players join
;      Game starts
;    Players move
;      When all players have moved, wait 3s for players to change their mind
;    Players see win/lose
;      Game restarts
;
; 2. ignore-new-mixin for Universe
;    ignore-msg-mixin for Universe
;    ignore-key-mixin for World
;
; 3. Queue players that join mid-game: queueing-mixin for Universe
;
; [4. Display countdown between states]

; Mixins:
;  - do-nothing on-new
;  - do-nothing on-msg
;  - queueing on-new

; Quirks:
;  - Only first tick-rate is used
;  - If initial world lacks on-key, none will ever be used
;  - If initial world has on-key, all worlds are expected to have on-key

(define WIDTH  300)
(define HEIGHT 300)

(define-interface none<%> [])

;
; Universes
;

(define-interface queueing<%> [queue])

(define (queue-new-mixin %)
  (class
    (super % none<%>)
    (fields queued)
    (define/public (on-new world)
      (begin
        (set-field! queued (cons world (queued)))
        (make-bundle this empty empty)))))

(define (ignore-msg-mixin %)
  (class
    (super % none<%>)
    (define/public (on-msg world)
      (make-bundle this empty empty))))

(define (new-joining-universe% worlds)
  (joining-universe% worlds))

(define joining-universe%
  ((compose ignore-msg-mixin)
   (class
     (fields worlds)

     (define/public (on-new world)
       (begin
         (set-field! worlds (cons world (worlds)))
         (make-bundle this empty empty)))

     (define/public (tick-rate) ; subsequent tick rates are ignored
       2)

     (define/public (on-tick)
       (make-bundle
         (new-playing-universe% (worlds))
         (map (λ (w) (make-mail w 'none))
              (worlds))
         empty))

     )))

(define (new-playing-universe% worlds)
  (playing-universe% empty worlds (set) (set)))

(define playing-universe%
  ((compose queue-new-mixin)
   (class
     (fields worlds lefts rights)

     (define/public (on-msg world x)
       (begin
         (when (equal? x "left")
           (set-field! lefts  (set-add    (lefts)  world))
           (set-field! rights (set-remove (rights) world)))
         (when (equal? x "right")
           (set-field! lefts  (set-remove (lefts)  world))
           (set-field! rights (set-add    (rights) world)))
         (make-bundle this empty empty)))

     (define/public (on-tick)
       (begin
         (printf "lefts  = ~s~n" (lefts))
         (printf "rights = ~s~n" (rights))
         (let ([l (set-count (lefts))]
               [r (set-count (rights))])
           (if (= (+ l r) (length (worlds)))
             (let ([minority (cond
                               [(< l r) (lefts)]
                               [(> l r) (rights)]
                               [else    (set)])])
               (make-bundle
                 (new-game-over-universe% (worlds) (this . queued))
                 (map (λ (w) (make-mail w (set-member? minority w)))
                      (worlds))
                 empty))
             (make-bundle this empty empty)))))

     )))

(define (new-game-over-universe% worlds queued)
  (game-over-universe% queued worlds))

(define game-over-universe%
  ((compose queue-new-mixin ignore-msg-mixin)
   (class
     (fields worlds)

     (define/public (on-tick)
       (make-bundle
         (new-joining-universe% (append (worlds) (this . queued)))
         (map (λ (w) (make-mail w 'none))
              (worlds))
         empty))

     )))

#|
(define waiting-universe%
  ((compose queue-new-mixin ignore-msg-mixin)
   (class
     (fields delay f)

     (define/public (on-tick)
       (begin
         (set-field! delay (- (delay) (tick-rate)))
         (if (<= delay 0)
           (f)
           (make-bundle this empty empty))))

     )))
|#

;
; Worlds
;

(define (ignore-key-mixin %)
  (class
    (super % none<%>)
    (define/public (on-key k)
      this)))

(define (new-joining-world%)
  (joining-world%))

(define joining-world%
  ((compose ignore-key-mixin)
   (class

     (define/public (register)
       LOCALHOST)

     (define/public (to-draw)
       (overlay
         (text "Joining..." 48 "black")
         (empty-scene WIDTH HEIGHT)))

     (define/public (on-receive none)
       (new-playing-world%))

     )))

(define (new-playing-world%)
  (playing-world% ""))

(define playing-world%
  ((compose)
   (class
     (fields choice)

     (define/public (to-draw)
       (overlay
         (text (choice) 48 "black")
         (empty-scene WIDTH HEIGHT)))

     (define/public (on-key k)
       (cond
         [(or (key=? k "left") (key=? k "right"))
          (begin
            (set-field! choice k)
            (make-package this (choice)))]
         [(key=? k "n")
          (begin
            (big-bang (new-world%))
            this)]
         [else this]))

     (define/public (on-receive win?)
       (new-game-over-world% win?))

     )))

(define (new-game-over-world% win?)
  (game-over-world% win?))

(define game-over-world%
  ((compose ignore-key-mixin)
   (class
     (fields win?)

     (define/public (to-draw)
       (overlay
         (text (if (win?) "Win!" "Lose...") 48 "black")
         (empty-scene WIDTH HEIGHT)))

     (define/public (on-receive none)
       (new-joining-world%))

     )))

;
; Go
;

(define (new-universe%)
  (new-joining-universe% empty))

(define (new-world%)
  (new-joining-world%))

(launch-many-worlds
  (universe (new-universe%))
  (big-bang (new-world%)))
