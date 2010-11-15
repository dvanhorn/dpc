#lang racket
(require 2htdp/image)

;; A Player is one of 'red or 'black
;; A Cell is one of Player or 'empty
;; A Column is (list Cell Cell Cell Cell Cell Cell)
;; columns are represented top = first
;; A Board is (list Column Column Column Column Column Column Column)
;; boards are represented leftmost = first
;; A Game is (game Player Board)
(struct game (next board))

(define COLS 7)
(define ROWS 6)

(define empty-col (make-list ROWS 'empty))
(define empty-board (make-list COLS empty-col))
(define initial-game (game 'red empty-board))

;; render : Board -> Image
(define (render board)
  (define (render-cell cell)
    (cond [(symbol=? cell 'black) (circle 20 "solid" "black")]
          [(symbol=? cell 'red) (circle 20 "solid" "red")]
          [else (circle 20 "outline" "black")]))
  (define (render-col col)
    (apply above (map render-cell col)))
  (apply beside (map render-col board)))

(define (list-set l k e)
  (cond [(= k 0) (cons e (rest l))]
        [else (cons (first l) (list-set (rest l) (sub1 k) e))]))

(define (play board col player)
  ;; col is never empty
  (define (play-col col player)
    (cond 
      [(not (symbol=? (first col) 'empty))
       (error 'play "column is full")]
      [(or (empty? (rest col))
           (symbol=? (second col) 'red)
           (symbol=? (second col) 'black))
       (cons player (rest col))]
      [else (cons (first col) (play-col (rest col) player))]))
  (list-set board col (play-col (list-ref board col) player)))

(define (flip-player pl)
  (cond [(symbol=? 'red pl) 'black]
        [else 'red]))

(define (play-turn g col)
  (game (flip-player (game-next g)) (play (game-board g) col (game-next g))))

(define (render-game g)
  (render (game-board g)))

