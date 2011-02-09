#lang class2
(require 2htdp/image class2/universe)

;; The Game 3-in-a-row
;; Red is the human player

(define SIZE 10)
(define WIN 3)

;; list-set : Listof[X] Nat X -> Listof[X]
;; replaces the kth element of l with e
(define (list-set l k e)
  (cond [(= k 0) (cons e (rest l))]
        [else (cons (first l) (list-set (rest l) (sub1 k) e))]))


;; A Play is one of
;; (empty%)
;; (black%)
;; (red%)
;; which implement 
;; win? : Number [Listof Play] -> Boolean
;; computer-win? : Number [Listof Play] -> Boolean
;; draw : -> Image

(define-class empty%
  (define/public (empty?) true)
  (define/public (computer?) false)
  (define/public (player?) false)
  (define/public (draw) (circle 20 "outline" "black")))
(define-class black%
  (define/public (empty?) false)
  (define/public (computer?) true)
  (define/public (player?) false)
  (define/public (draw) (circle 20 "solid" "black")))
(define-class red%
  (define/public (empty?) false)
  (define/public (computer?) false)
  (define/public (player?) true)
  (define/public (draw) (circle 20 "solid" "red")))

;; A World is a (new world% [Listof Play] Computer) of length SIZE
(define-class world%
  (fields board computer)
  (define/public (to-draw)
    (overlay
     (apply beside (map (λ (e) (send e draw)) (field board)))
     (empty-scene 500 500)))
  (define/public (on-key k)
    (cond [(number? (string->number k)) (play (string->number k))]
          [(key=? "n" k) (new world% INITIAL (field computer))]))
  (define/public (play n)
    (local [(define new-board (list-set (field board) n (red%)))
            (define new-comp (send (field computer) record-player n new-board))]
      (cond [(player-win? new-board)
             (done-world% "You Win!" new-comp)]
            [(tie? new-board)
             (done-world% "Tie!" new-comp)]
            [else 
             (local [(define cpick (send new-comp pick new-board))                     
                     (define new2-board (list-set new-board cpick (black%)))
                     (define new2-comp (send new-comp record-self cpick new2-board))]
               (cond [(computer-win? new2-board)
                      (done-world% "Computer Win!" new2-comp)]            
                     [(tie? new2-board)
                      (done-world% "Tie!" new2-comp)]
                     [else
                      (world% new2-board new2-comp)]))]))))
  


(define (win-player? b n)
  (cond 
    [(= n WIN) true]
    [(empty? b) false]
    [(send (first b) player?) (win-player? (rest b) (add1 n))]
    [else (win-player? (rest b) 0)]))

(define (win-computer? b n)
  (cond 
    [(= n WIN) true]
    [(empty? b) false]
    [(send (first b) computer?) (win-computer? (rest b) (add1 n))]
    [else (win-computer? (rest b) 0)]))

(define (tie? b) (false? (first-empty b)))
(define (player-win? b) (win-player? b 0))  
(define (computer-win? b) (win-computer? b 0))

(define-class done-world%
  (fields status computer)
  (define/public (to-draw)
    (overlay (text (field status) 24 "blue")
             (empty-scene 500 500)))
  (define/public (on-key k)
    (cond [(key=? "n" k) (new world% INITIAL (field computer))]
          [else this])))

(define INITIAL 
  (build-list SIZE (λ (x) (empty%))))

;; find the index of the first, or false
(define (first-empty b)
  (cond 
    [(empty? b) false]
    [(send (first b) empty?) 0]
    [(number? (first-empty (rest b)))
     (add1 (first-empty (rest b)))]
    [else false]))

(define-class computer%
  ;; remember this board and the move we made
  ;; Board Number -> Computer
  (define/public (record-player b m) this)
  (define/public (record-self b m) this)
  ;; pick a move
  ;; Board -> Number
  (define/public (pick b)
    (first-empty b)))

(big-bang (new world% INITIAL (computer%)))