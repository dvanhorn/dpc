#lang class2
(require 2htdp/image class2/universe)


;; A Player is one of "red" or "black"
;; A Cell is one of Player or #f
;; A Column is [Listof Cell]
;; columns are represented bottom = first
;; A Board is [Listof Column]
;; boards are represented leftmost = first

(define COLS 7)
(define ROWS 6)
(define WIN 4)

;; we ASSUME that there is only one column in which diagonals can win in *all* positions
;; this is true for 4x4 with WIN>=3, and 7x6 with WIN>=4
;; I don't have a general formula yet.

(define RADIUS 20)

(define SIZE 400)

(define left-margin (/ (- SIZE (* 2 RADIUS COLS)) 2))

(define empty-col (make-list ROWS #f))
(define empty-board (make-list COLS empty-col))

;; render : Board -> Image
;; create a picture representing the board state
(define (render board)
  (local [(define (render-cell cell)
            (cond [(false? cell) (circle RADIUS "outline" "black")]
                  [(string=? cell "black") (circle RADIUS "solid" "black")]
                  [(string=? cell "red") (circle RADIUS "solid" "red")]))
          (define (render-col col)
            (apply above (reverse (map render-cell col))))]
    (apply beside (map render-col board))))

;; A World is one of
;; - (start-world%)
;; - (done-world% String)
;; - (world% Board Color Boolean)

(define-class start-world%  
  (define/public (register) LOCALHOST)
  (define/public (to-draw)
    (overlay (text "Waiting ..." 20 "black")
             (empty-scene SIZE SIZE)))
  (check-expect ((start-world%) . on-mouse "m" 0 0) (start-world%))
  (define/public (on-mouse m x y) this)
  (check-expect ((start-world%) . on-receive "red") (world% empty-board "red" true))
  (check-expect ((start-world%) . on-receive "black") (world% empty-board "black" false))
  (define/public (on-receive m)
    (world% empty-board m (string=? m "red"))))

(define-class world%
  (fields board color my-turn?)
  (check-expect ((world% empty-board "black" false) . on-receive "lose") (done-world% "We Lost!"))
  (check-expect ((world% empty-board "black" false) . on-receive "win") (done-world% "We Won!"))
  (check-expect ((world% empty-board "black" false) . on-receive empty-board)
                (world% empty-board "black" true))
  ;; on-recieve : Message -> World
  ;; handle messages - either a new board, or "win" or "lose"
  (define/public (on-receive m)
    (cond [(and (string? m) (string=? m "lose")) (done-world% "We Lost!")]
          [(and (string? m) (string=? m "win")) (done-world% "We Won!")]
          [else (world% m (field color) (not (field my-turn?)))]))
  ;; draw the board
  ;; -> Scene
  (define/public (to-draw)
    (overlay (above (render (field board))
                    (text (field color) 15 (field color)))
             (empty-scene SIZE SIZE)))
  ;; Handle mouse events
  ;; button clicks play in the appropriate column
  ;; others are ignored
  (check-expect ((world% empty-board "red" true) . on-mouse 100 100 "button-up")
                (make-package (world% (play empty-board 1 "red") "red" false)
                              1))
  (check-expect ((world% empty-board "red" true) . on-mouse 100 100 "button-down")
                (world% empty-board "red" true))
  (check-expect ((world% empty-board "red" true) . on-mouse 0 0 "button-up")
                (world% empty-board "red" true))
  (define/public (on-mouse x y e)
    (cond [(mouse=? e "button-up")
           (local [(define col (quotient (- x left-margin) (* 2 RADIUS)))]           
             (cond 
               [(and (field my-turn?) (> x left-margin) (<= 0 col (sub1 COLS)))
                (make-package (world% (play (field board) col (field color)) (field color) false)
                              col)]
               [else this]))]
          [else this])))


(define-class done-world%
  (fields message)  
  
  (check-expect ((done-world% "hi") . to-draw) (overlay (text "hi" 20 "black")
                                                        (empty-scene SIZE SIZE)))
  ;; render the message
  ;; -> Scene
  (define/public (to-draw)
    (overlay (text (field message) 20 "black")
             (empty-scene SIZE SIZE)))
  
  (check-expect ((done-world% "hi") . on-mouse "mouse-up" 1 1) (done-world% "hi"))  
  ;; handle mouse events
  ;; this is only needed b/c it might be called even if it doesn't exist
  ;; -> World
  (define/public (on-mouse m x y) this))

;; flip-player : Player -> Player
;; get the next player
(check-expect (flip-player "black") "red")
(check-expect (flip-player "red") "black")
(define (flip-player pl)
  (cond [(string=? "red" pl) "black"]
        [else "red"]))

(define-class initial-universe%
  (check-expect ((initial-universe%) . on-msg iworld1 0) (initial-universe%))
  (define/public (on-msg i m) this)
  (check-expect ((initial-universe%) . on-new iworld1) (make-bundle (universe-one% iworld1) empty empty))
  (define/public (on-new i) (make-bundle (universe-one% i) empty empty)))

(define-class universe-one%
  (fields player)
  (check-expect ((universe-one% iworld1) . on-msg iworld1 0) (universe-one% iworld1))
  (check-expect ((universe-one% iworld1) . on-msg iworld2 0) (universe-one% iworld1))
  (define/public (on-msg i m) this)
  (check-expect ((universe-one% iworld1) . on-new iworld2)
                (make-bundle (universe% empty-board iworld1 iworld2 "red") 
                             (list (make-mail iworld1 "red")
                                   (make-mail iworld2 "black"))
                             empty))
  (define/public (on-new i)
    (make-bundle (universe% empty-board (field player) i "red")
                 (list (make-mail (field player) "red")
                       (make-mail i "black"))
                 empty)))


;; list-set : Listof[X] Nat X -> Listof[X]
;; replaces the kth element of l with e
(define (list-set l k e)
  (cond [(= k 0) (cons e (rest l))]
        [else (cons (first l) (list-set (rest l) (sub1 k) e))]))
(check-expect (list-set '(1 2 3) 1 4) '(1 4 3))

;; play : Board Nat Player -> Board or #f
;; player plays on `board' in the `col'th column
(define (play board col player)
  (local
    ;; play-col : Column Player -> Column or #f
    ;; add player's chip on top of `col', returns #f if `col' is full 
    [(define (play-col col player)
       (cond [(empty? col)
              #f]
             [(false? (first col))
              (cons player (rest col))]
             [else
              (let ([p (play-col (rest col) player)])
                (if (false? p) false (cons (first col) p)))]))
     (define new-col (play-col (list-ref board col) player))]
    (cond [(false? new-col) false]
          [else (list-set board col new-col)])))

(check-expect (play empty-board 0 "red")
              '(("red" #f #f #f #f #f)
                (#f #f #f #f #f #f)
                (#f #f #f #f #f #f)
                (#f #f #f #f #f #f)
                (#f #f #f #f #f #f)
                (#f #f #f #f #f #f)
                (#f #f #f #f #f #f)))
(check-expect (play (play empty-board 0 "red") 0 "black")
              '(("red" "black" #f #f #f #f)
                (#f #f #f #f #f #f)
                (#f #f #f #f #f #f)
                (#f #f #f #f #f #f)
                (#f #f #f #f #f #f)
                (#f #f #f #f #f #f)
                (#f #f #f #f #f #f)))

(define-class universe%
  (fields board red black next)
  ;; Color -> IWorld
  ;; convert a color to the world that plays that color
  (define/public (color->iworld c)
    (cond [(string=? c "red") (field red)]
          [else (field black)]))
  ;; winner? : Game -> Boolean
  ;; has someone won the game?
  (define/public (winner? b)
    (or (win-cols? b) (win-rows? b) (win-riser? b) (win-sinker? b)))
  (define/public (on-new i) this)
  (define/public (on-msg i m)
    (cond [(iworld=? i (color->iworld (field next)))
           (local [(define bnew (play (field board) m (field next)))]
             (cond [(false? bnew) (make-bundle this empty empty)]
                   [(winner? bnew)
                    (make-bundle this
                                 (list (make-mail (color->iworld (field next)) "win")
                                       (make-mail (color->iworld (flip-player (field next))) "lose"))
                                 empty)]
                   [else 
                    (make-bundle (universe% bnew
                                            (field red) (field black)
                                            (flip-player (field next)))
                                 (list (make-mail (color->iworld (flip-player (field next))) bnew))
                                 empty)]))])))

;; stuff for winning:

;; is there a winner in column c (of either color)
;; Col -> Bool
(define (winner-col? c)
  (define (loop c cnt color)
    (cond 
      [(= WIN cnt) true]
      [(empty? c) false]
      [(false? (first c)) (loop (rest c) 0 false)]
      [(and (not (false? color)) (string=? color (first c))) (loop (rest c) (add1 cnt) color)]
      [else (loop (rest c) 1 (first c))]))
  (loop c 0 false))

(check-expect (winner-col? empty-col) #f)
(check-expect (winner-col? '(#f #f "red" "red" "red" "red")) #t)

;; Board -> Board^T
(define (transpose b) (apply map list b))

;; cons to the end of a list
(define (snoc v l) (append l (list v)))

(define (take l i)
  (cond [(empty? l) l]
        [(= 0 i) empty]
        [else (cons (first l)
                    (take (rest l) (sub1 i)))]))

(define (drop l i)
  (cond [(empty? l) l]
        [(= 0 i) l]
        [else (drop (rest l) (sub1 i))]))

;; shift the view of column up i squares
;; this imagines an infinite board padded w/ #f
;; Col Int -> Col
(define (adjust-column c i)  
  (cond 
    [(< i 0) (append (make-list (abs i) #f) (take c (+ ROWS i)))]
    [(= i 0) c]
    [else (append (drop c i) (make-list i #f))]))

;; Board -> Board^T
;; produces a board where diagonals go across
(define (diagonalize b)
  (local [(define small (quotient COLS 2))]
    (map (λ (i c) (adjust-column c i))
         (build-list COLS (λ (i) (- i small)))
         b)))

;; is there a winner in some column?
(define (win-cols? b) (ormap winner-col? b))
;; is there a winner in some row?
(define (win-rows? b) (win-cols? (transpose b)))
;; is there a winner in some rising diagonal
(define (win-riser? b) (win-rows? (diagonalize b)))
;; is there a winner in some sinking diagonal?
(define (win-sinker? b) (win-riser? (reverse b)))

(define (go)
  (launch-many-worlds (big-bang (start-world%))
                      (big-bang (start-world%)))
                    (universe (initial-universe%)))