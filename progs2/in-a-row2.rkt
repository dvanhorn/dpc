#lang class2
(require 2htdp/image class2/universe)

;; A Board is a [Listof Color]
;; A Color is one of
;; - 'white - blank
;; - 'black - the computer
;; - 'red - the player

(define WIN 3)
(define SIZE 10)

;; player-win? : Board -> Boolean
(define (player-win? b)
  (win-helper 'red b 0))

;; computer-win? : Board -> Boolean
(define (computer-win? b)
  (win-helper 'black b 0))

;; tie: Board -> Boolean
(define (tie? b)
  (not (member 'white b)))

;; win-helper : Color Board Number ->  Boolean
;; Acc : how many pieces we've seen so far  
(define (win-helper sym b acc)
  (cond [(= acc WIN) true]
        [(empty? b) false]
        [else (cond [(symbol=? sym (first b))
                     (win-helper sym (rest b) (add1 acc))]
                    [else (win-helper sym (rest b) 0)])]))

;; play : Color Number Board -> Board
;; place the appropriate piece on the board
(define (play c n b)
  (cond 
    [(> n (add1 SIZE)) b]
    [(symbol=? 'white (list-ref b n))
     (list-set b n c)]
    [else b]))


;; list-set : Listof[X] Nat X -> Listof[X]
;; replaces the kth element of l with e
(define (list-set l k e)
  (cond [(= k 0) (cons e (rest l))]
        [else (cons (first l) (list-set (rest l) (sub1 k) e))]))

;; find-res : Evaluation Result -> Number or False
;; find the first occurence of this result
(define (find-res eval r)
  (cond [(empty? eval) false]
        [else (cond [(symbol=? r (first eval)) 0]
                    [else 
                     (local [(define result (find-res (rest eval) r))]
                       (cond [(number? result) (add1 result)]
                             [else false]))])]))

;; A Computer implements:
;; pick : Board -> Number
(define-class computer%
  
  ;; An Evaluation is a [Listof Result]
  ;; A Result is one of 'Win 'Block 'Full 'Unk
  
  ;; evaluate Board -> Evaluation
  (define/public (evaluate brd)
    (local [(define (evaluate-elem index)
              (cond [(not (symbol=? 'white (list-ref brd index))) 'Full]
                    [(computer-win? (play 'black index brd)) 'Win]
                    [(player-win? (play 'red index brd)) 'Block]
                    [else 'Unk]))]
      (build-list SIZE evaluate-elem)))
  
  ;; Pick a space to play in
  ;; Invariant : brd is not full
  ;; Board -> Number
  (define/public (pick brd)
    (local [(define eval (evaluate brd))]
      (cond [(member 'Win eval)
             (find-res eval 'Win)]
            [(member 'Block eval)
             (find-res eval 'Block)]
            [else
             (find-res eval 'Unk)]))))

(define SCENE (empty-scene 500 500))

;; A World is (world% Board Computer)
(define-class world%
  (fields board computer)
  (define/public (to-draw)
    (overlay
     (foldr beside empty-image (map (λ (clr) (circle 20 "solid" clr)) (field board)))
     SCENE))
  
  (define/public (on-key k)
    (cond [(number? (string->number k)) 
           (local [(define new-board (play 'red (string->number k) (field board)))]
             (cond [(player-win? new-board) (done% "Player Win!" (field computer))]
                   [(tie? new-board) (done% "Tie!" (field computer))]
                   [else
                    (local [(define second-board (play 'black 
                                                       (send (field computer) pick new-board)
                                                       new-board))]
                      (cond [(computer-win? second-board) (done% "Computer Win!" 
                                                                 (field computer))]
                            [(tie? second-board) (done% "Tie!" (field computer))]
                            [else (world% second-board (field computer))]))]))]
          [(key=? "n" k) (world% INITIAL (field computer))]
          [else this]))
  )

;; A DoneWorld is (done% String Computer)
(define-class done%
  (fields msg computer)
  
  (define/public (to-draw)
    (overlay (text (field msg) 30 "black")
             SCENE))
  
  (define/public (on-key k)
    (cond [(key=? "n" k) (world% INITIAL (field computer))]
          [else this])))

(define INITIAL (build-list SIZE (λ (i) 'white)))

(big-bang (new world% INITIAL (computer%)))








