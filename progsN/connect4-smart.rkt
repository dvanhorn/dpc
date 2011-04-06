#lang class3

(require "connect4-universe.rkt" class3/universe 2htdp/image)

;; Computer Player Worlds
;; A SWorld is one of
;; - (start-sworld%)
;; - (done-sworld% String)
;; - (sworld% Board Color Boolean [Listof Board] [Maybe Number] [Maybe Board])

(define-class start-sworld%
  (fields random)
  (check-expect ((start-sworld% true) . register) LOCALHOST)
  (define/public (register) LOCALHOST)
  (define/public (name) 
    (if (random) "Random Smart Connect 4" "Smart Connect 4"))
  (define/public (to-draw)
    (overlay (text "Waiting ..." 20 "black")
             (empty-scene SIZE SIZE)))
  (define/public (on-receive m)
    (cond [(string=? m "red") 
           (local [(define w (sworld% empty-board m empty false false (list 0 0 0) (random)))]
             (send w on-receive empty-board))]
          [else (sworld% empty-board m empty false false (list 0 0 0) (random))])))

;; An Evaluation is Listof[One of 'full 'win 'lose 'unknown]
;; A Status is (cons Board Evaluation)

(define (win score)
  (list (add1 (first score))
        (second score)
        (third score)))

(define (lose score)
  (list (first score)
        (add1 (second score))
        (third score)))

(define (tie score)
  (list (first score)
        (second score)
        (add1 (third score))))

;; choose a random element of l, or #f if empty
;; Listof[X] -> X or f
(define (choose r l) (if (null? l) #f (list-ref l (if r (random (length l)) 0))))

  
(define (random-index-of r status sym)
  (local [(define (help l n)
            (cond [(empty? l) l]
                  [(symbol=? sym (first l)) (cons n (help (rest l) (add1 n)))]
                  [else (help (rest l) (add1 n))]))]
    (choose r (help status 0))))

;; does the next player to play in `g' have a possible winning move?
;; Game Player -> Bool
(define (has-winning-move? g next)
  (ormap
   (λ (i) (and (not (full? (list-ref g i))) (winner? (play g i next))))
   (build-list (length g) (λ (j) j))))

(define (assoc v l)
  (cond [(empty? l) false]
        [(equal? v (first (first l))) (first l)]
        [else (assoc v (rest l))]))

(define (update-bad mem board move)
  (local [(define old-eval (second (assoc board mem)))
          (define new-eval (list-set old-eval move 'lose))]
    (cons (list board new-eval) mem)))


(define-class sworld%
  (fields board color memory last-move last-board score random)
  
  (define/public (to-draw)
    (overlay 
     #;(text (field color) 15 (field color))
     (above (render (field board))
            (text (field color) 15 (field color)))
     (empty-scene SIZE SIZE)))
  
  (define/public (on-receive m)
    (cond [(and (string? m) (string=? m "lose")) (done-sworld% "We Lost!" (memory) (lose (score)) (random))]
          [(and (string? m) (string=? m "win")) (done-sworld% "We Won!" (memory) (win (score)) (random))]
          [(and (string? m) (string=? m "tie")) (done-sworld% "We Tied." (memory) (tie (score)) (random))]
          [else 
           (local [(define old-eval (assoc m (memory)))
                   (define eval (if (cons? old-eval)
                                    (second old-eval)
                                    (evaluate m (color))))
                   (define col (pick-move eval))
                   (define bad-situation?
                     (symbol=? 'lose (list-ref eval col)))
                   (define new-b (play m col (color)))
                   (define new-mem
                     (if (cons? old-eval) 
                         (memory)
                         (cons (list m eval) (memory))))
                   (define new-mem2
                     (if bad-situation? 
                         (update-bad new-mem (last-board) (last-move))
                         new-mem))]
             (make-package (sworld% new-b (field color) new-mem2 col m (score) (random))
                           col))]))
  
  
    
  ;; choose the best move in `status'
  (define/public (pick-move status)
    (local [(define win (random-index-of (random) status 'win))
            (define unk (random-index-of (random) status 'unknown))
            (define lose (random-index-of (random) status 'lose))]
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


(define-class done-sworld%
  (fields message memory score random)  
    
  ;; render the message
  ;; -> Scene
  (define/public (to-draw)
    (overlay (above (text (field message) 20 "black")
                    (text (format "~a" (field score)) 20 "black"))
             (empty-scene SIZE SIZE)))
  
  (define/public (on-receive m)
    (cond [(string=? m "red") 
           (local [(define w (sworld% empty-board m (memory) false false (score) (random)))]
             (send w on-receive empty-board))]
          [else (sworld% empty-board m (memory) false false (score) (random))])))

(define (go2)  
  (launch-many-worlds (big-bang (start-sworld% false))
                      (big-bang (start-world%))
                      (universe (initial-universe%))))
