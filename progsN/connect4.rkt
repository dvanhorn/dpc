#lang racket
(require 2htdp/image 2htdp/universe)

;; A Player is one of 'red or 'black
;; A Cell is one of Player or #f
;; A Column is (list Cell Cell Cell Cell Cell Cell)
;; columns are represented bottom = first
;; A Board is (list Column Column Column Column Column Column Column)
;; boards are represented leftmost = first
;; A Game is (game Player Board)
(struct game (next board))

(define COLS 7)
(define ROWS 6)

(define empty-col (make-list ROWS #f))
(define empty-board (make-list COLS empty-col))
(define initial-game (game 'red empty-board))

;; render : Board -> Image
;; create a picture representing the board state
(define (render board)
  (define (render-cell cell)
    (cond [(not cell) (circle 20 "outline" "black")]
          [(symbol=? cell 'black) (circle 20 "solid" "black")]
          [(symbol=? cell 'red) (circle 20 "solid" "red")]))
  (define (render-col col)
    (apply above (reverse (map render-cell col))))
  (apply beside (map render-col board)))

;; list-set : Listof[X] Nat X -> Listof[X]
;; replaces the kth element of l with e
(define (list-set l k e)
  (cond [(= k 0) (cons e (rest l))]
        [else (cons (first l) (list-set (rest l) (sub1 k) e))]))

;; play : Board Nat Player -> Board or #f
;; player plays on `board' in the `col'th column
(define (play board col player)
  ;; play-col : Column Player -> Column or #f
  ;; add player's chip on top of `col', returns #f if `col' is full 
  (define (play-col col player)
    (cond [(empty? col)
           #f]
          [(not (first col))
           (cons player (rest col))]
          [else
           (let ([p (play-col (rest col) player)])
             (and p (cons (first col) p)))]))
  (define new-col (play-col (list-ref board col) player))
  (if new-col
      (list-set board col new-col)
      #f))

;; flip-player : Player -> Player
;; get the next player
(define (flip-player pl)
  (cond [(symbol=? 'red pl) 'black]
        [else 'red]))

;; play-turn : Game Nat -> Game
;; current player plays in `col', producing a new game state
;; if the move is illegal, game state doesn't change
(define (play-turn g col)
  (define new-board (play (game-board g) col (game-next g)))
  (if new-board
      (game (flip-player (game-next g)) new-board)
      g))

(define (winner-col? c)
  (let loop ([c c] [cnt 0] [color #f])
    (cond 
      [(= 4 cnt) #t]
      [(empty? c) #f]
      [(not (first c)) (loop (rest c) 0 #f)]
      [(and color (symbol=? color (first c))) (loop (rest c) (add1 cnt) color)]
      [else (loop (rest c) 1 (first c))])))

(define (transpose b)
  (apply map list b))

;; winner? : Game -> Boolean
;; has someone won the game?
(define (winner? g)
  (or (ormap winner-col? (game-board g))
      (ormap winner-col? (transpose (game-board g)))))

;; Game -> Scene
(define (render-game g)
  (place-image
   (render (game-board g))
   200 150
   (place-image
    (cond [(symbol=? (game-next g) 'black) (circle 20 "solid" "black")]
          [(symbol=? (game-next g) 'red) (circle 20 "solid" "red")])
    200 350
    (empty-scene 400 400))))

;; Game KeyEvent -> Game
;; play in the column denoted by `k'
(define (handle-key g k)
  (define n (string->number k))
  (if (and n (<= 1 n 7))
      (play-turn g (sub1 n))
      g))

;; Game -> Scene
(define (render-winner g)
  (define w (flip-player (game-next g)))
  (define s (render-game g))
  (define str
    (cond [(symbol=? w 'black) (text "Black Player Wins!" 36 "green")]
          [(symbol=? w 'red) (text "Red Player Wins!" 36 "green")]))
  (overlay str s))

(big-bang
 initial-game
 (to-draw render-game)
 (on-key handle-key)
 (stop-when winner? render-winner))

