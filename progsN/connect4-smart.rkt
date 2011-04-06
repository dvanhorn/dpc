#lang class3

(require "connect4-universe.rkt" class3/universe 2htdp/image)

;; Computer Player Worlds
;; A SWorld is one of
;; - (start-sworld%)
;; - (done-sworld% String)
;; - (sworld% Board Color Boolean [Listof Board] [Maybe Number] [Maybe Board])

(define-class start-sworld%
  (check-expect ((start-sworld%) . register) LOCALHOST)
  (define/public (register) LOCALHOST)
  (define/public (to-draw)
    (overlay (text "Waiting ..." 20 "black")
             (empty-scene SIZE SIZE)))
  (define/public (on-receive m)
    (cond [(string=? m "red") 
           (local [(define w (sworld% empty-board m empty false false))]
             (send w on-receive empty-board))]
          [else (sworld% empty-board m empty false false)])))

;; An Evaluation is Listof[One of 'full 'win 'lose 'unknown]
;; A Status is (status% Board Evaluation)
(define-class status%
  (fields board eval))

(define random? true)

;; choose a random element of l, or #f if empty
;; Listof[X] -> X or f
(define (choose l) (if (null? l) #f (list-ref l (if random? (random (length l)) 0))))

  
(define (random-index-of status sym)
  (local [(define (help l n)
            (cond [(empty? l) l]
                  [(symbol=? sym (first l)) (cons n (help (rest l) (add1 n)))]
                  [else (help (rest l) (add1 n))]))]
    (choose (help status 0))))

;; does the next player to play in `g' have a possible winning move?
;; Game Player -> Bool
(define (has-winning-move? g next)
  (ormap
   (λ (i) (and (not (full? (list-ref g i))) (winner? (play g i next))))
   (build-list (length g) (λ (j) j))))


(define-class sworld%
  (fields board color memory last-move last-board)
  
  (define/public (to-draw)
    (overlay (above (render (field board))
                    (text (field color) 15 (field color)))
             (empty-scene SIZE SIZE)))
  
  (define/public (on-receive m)
    (cond [(and (string? m) (string=? m "lose")) (done-sworld% "We Lost!" (memory))]
          [(and (string? m) (string=? m "win")) (done-sworld% "We Won!" (memory))]
          [else 
           (local [(define col (pick-move m))
                   (define new-b (play m col (color)))]
             (make-package (sworld% new-b (field color) (memory) (last-move) (last-board))
                           col))]))
  
  
    
  ;; choose the best move in `g' with one-ply lookahead
  (define/public (pick-move g)
    (local [(define status (evaluate g (color)))
            (define win (random-index-of status 'win))
            (define unk (random-index-of status 'unknown))
            (define lose (random-index-of status 'lose))]
      (cond [(number? win) win]
            [(number? unk) unk]
            [else lose])))
  
  ;; produce an evaluation of the state of `g' for the next player
  ;; Game -> Listof[One of 'full 'win 'lose 'unknown]
  (define/public (evaluate g next)
    (build-list 
     (length g)
     (λ (i)
       (cond [(full? (list-ref g i)) 'full]
             [(winner? (play g i next)) 'win]
             [(has-winning-move? (play g i next) (flip-player next)) 'lose]
             [else 'unknown])))))

;; Is this column full?
(define (full? col)
  (not (member #f col)))

(define-class done-sworld%
  (fields message memory)  
    
  ;; render the message
  ;; -> Scene
  (define/public (to-draw)
    (overlay (text (field message) 20 "black")
             (empty-scene SIZE SIZE)))
  
  (define/public (on-receive m)
    (cond [(string=? m "red") 
           (local [(define w (sworld% empty-board m (memory) false false))]
             (send w on-receive empty-board))]
          [else (sworld% empty-board m (memory) false false)])))

(define (go2)  
  (launch-many-worlds (big-bang (start-world%))
                      (big-bang (start-sworld%))
                      (universe (initial-universe%))))
