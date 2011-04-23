#lang class5
(require class5/universe 2htdp/image)
(require (except-in racket #%app))

(define WIDTH 7)
(define HEIGHT 5)

(define MAXDICE 10)

;; A BPosn is a (list [0,WIDTH) [0,HEIGHT))

;; A BoardRep is a [Listof (list Number [Listof BPosn])]

;; Interp: a BoardRep is a list of territories, where each
;; territory is owned by a player (indicated by number)
;; and consists of a set of continguous board positions.

;; Invariants:
;; - Each list of positions is disjoint from the other lists.
;; - Each list of positions is contiguous.
;; - Each list of positions consists of distinct elements.
;; - All of territories are contiguous.

(define sample-board
  '((1 ((0 2) (0 3) (1 2) (1 3)))
    (2 ((0 0) (0 1)))
    (2 ((1 4) (2 4) (2 3)))
    (1 ((2 0) (3 0) (4 0) (5 0) (5 1)))
    (1 ((1 1) (2 1) (2 2) (3 2)))
    (2 ((3 3) (4 3) (4 2) (5 2) (6 2)))
    (2 ((6 0) (6 1)))
    (1 ((4 4) (5 4) (5 3) (6 4) (6 3)))))


;; Coord -> Boolean
(define (valid-coord? p)
  (match p
    [(list i j) (and (< -1 i WIDTH)
                     (< -1 j HEIGHT))]))

;; Coord -> [Listof Coord]
(define (adjacent-coords p)
  (match p 
    [(list i j)
     (filter valid-coord?
             (list (list (sub1 i) j)
                   (list (add1 i) j)
                   (list i (sub1 j))
                   (list i (add1 j))))]))

(check-expect (adjacent-coords '(0 0))
              (list '(1 0) '(0 1) ))

;; [Listof Coord] -> [Listof Coord]
;; Assume ps adjacent.
(define (region-adjacent-coords ps)
  (remove-duplicates (filter (λ (p) (not (member p ps)))
                             (append-map adjacent-coords ps))))

;; Coord Board -> Number or False
;; Determine region coordinate is in.
(define (find-region p b)
  (for/first ([(r i) (in-indexed b)]
              #:when (member p (second r)))
    i))

(check-expect (find-region '(0 1) sample-board) 1)
(check-expect (find-region '(6 3) sample-board) 7)

;; Number Board -> [Listof Number]
;; Find indices for all adjacent regions.
(define (adjacent-regions i b)
  (sort (remove-duplicates (filter-map (λ (p) (find-region p b))
                                       (region-adjacent-coords (second (list-ref b i)))))
        <))

(check-expect (adjacent-regions 1 sample-board)
              (list 0 4))
(check-expect (adjacent-regions 7 sample-board)
              (list 5))
(check-expect (adjacent-regions 4 sample-board)
              (list 0 1 2 3 5))
  

(define (random-dice)
  (add1 (random MAXDICE)))

(define (list-set l k v)
  (append (take l k)
          (list v)
          (drop l (add1 k))))
(check-expect (list-set '(1 2 3) 0 4) '(4 2 3))
(check-expect (list-set '(1 2 3) 2 4) '(1 2 4))


;; A SerialBoard is
;; (List [Listof SerialPlayer]
;;       [Listof SerialRegion])

;; A SerialRegion is (List [Listof Number] SerialPlayer Number)

;; A SerialPlayer is (List Number String)

(define-class board%
  (fields players regions observers)
  (define/public (serialize)
    (list (map (λ (p) (p . serialize)) (players))
          (map (λ (r) (r . serialize)) (regions))))
  
  (define/public (current-player)
    (first (players)))
  
  (define/public (get-region i)
    (list-ref (regions) i))
  
  (define/public (do-attack src-idx dst-idx)
    (let ()
      (define src (get-region src-idx))
      (define dst (get-region dst-idx))
      (define src-roll (src . roll))
      (define dst-roll (dst . roll))
      (attack-result%
       src-roll dst-roll
       (board% 
        (players)             
        (cond [(> (apply + src-roll) (apply + dst-roll))
               (list-set (list-set (regions) src-idx (src . set-dice 1)
                                   dst-idx
                                   (dst . set-player+dice (src . player) (sub1 (src . dice)))))]
              [else (list-set (regions)
                              src-idx
                              (src . set-dice 1))])
        (observers)))))
  
  (define/public (rotate-players)
    (board% (append (rest (players))
                    (list (first (players))))
            (regions)
            (observers))))

(define-class attack-result%
  (fields offense defense new-board))


(define (make-player iw n)
  (player% iw (format "Player ~a" n) n))

(define-class player%
  (fields iworld name number)
  (define/public (serialize)
    (list (number) (name))))

(define (make-regions sb ps)
  (for/list ([(r i) (in-indexed sb)])
    (region% (adjacent-regions i sb) 
             (hash-ref ps (first r)) 
             (random-dice))))

(define-class region%
  (fields neighbors player dice)
  (define/public (set-dice n)
    (region% (neighbors) (player) n))  
  (define/public (set-player+dice p n)
    (region% (neighbors) p n))
  (define/public (roll)
    (for/list ([i (in-range (dice))])
      (+ 1 (random 6))))
  (define/public (serialize) 
    (list (neighbors) ((player) . serialize) (dice))))

(define-class start%
  (fields num-players)
  (define/public (on-new iw) 
    (waiting% (sub1 (num-players)) 
              (hash iw (make-player iw (num-players)))))
  
  (define/public (on-msg iw msg) 'cantpossiblyoccur))
  
(define-class waiting%
  (fields num-players players)
  (define/public (on-new iw)
    (cond [(= 1 (num-players)) 
           (let* ((ps (hash-set (players) iw (make-player iw (num-players))))
                  (n->p (for/hash ([(iw p) ps])
                          (values (p . number) p)))                                   
                  (b  (board% (hash-values ps) 
                              (make-regions sample-board n->p)
                              empty))                  
                  (ms (for/list ([(iw p) ps])
                        (make-mail iw (list 'start 
                                            (p . number) 
                                            (b . serialize)
                                            sample-board))))
                  (t (for/first ([(iw p) ps])
                       (make-mail iw 'turn))))
             
             (make-bundle (playing% ps b)
                          (append ms (list t))
                          empty))]
          [else
           (waiting% (sub1 (num-players))
                    (hash-set (players) iw (make-player iw (num-players))))]))
  
  (define/public (on-msg iw msg) 
    (match msg
      [(list 'name name) 
       (waiting% (num-players) (hash-set (players) iw (player% iw name)))]
      [_ this])))



(define-class playing%
  (fields players board)
  (define/public (on-new iw) this)
  (define/public (on-msg iw msg)
    (let ()
      (define-match-expander turn
        (syntax-rules ()
          [(_ p)
           (and p
                (? (λ x (iworld=? iw ((board) . current-player . iworld)))))]))
      (match msg
        [(turn 'done)
         (next-turn)]
        [(turn (list 'attack (? integer? src) (? integer? dst)))
         (attack src dst)]
        [_ (make-bundle this
                        (list (make-mail iw 'error))
                        empty)])))
  
  (define/public (attack src dst)
    (let ()
      (define cur ((board) . current-player))
      (define src-r ((board) . get-region src))
      (define dst-r ((board) . get-region dst))
      (cond [(and src-r dst-r (equal? cur (src-r . player)) (< 1 (src-r . dice)))
             (let ([result ((board) . do-attack src dst)])
               (make-bundle (playing% (players)
                                     (result . new-board))
                            (for/list ([(iw p) (players)])
                              (make-mail iw
                                       `(attack ,src ,(result . offense) 
                                                ,dst (result . defense) 
                                                ,(result . new-board . serialize))))))]
            [else 
             (make-bundle this
                          (list (make-mail (cur . iworld) 'illegal))
                          empty)])))
  
  (define/public (next-turn)
    ;; allocate dice
    (let ()
      (define new-board ((board) . rotate-players))
      (define next (new-board . current-player))
      (make-bundle (playing% (players) new-board)
                   (append (broadcast-state) 
                           (list (make-mail (next . iworld) 'turn)))
                   empty)))
  
  ;; -> [Listof Mail]
  (define/public (broadcast-state)
    (for/list ([(iw p) (players)])
      (make-mail iw (list 'new-state
                          ((board) . serialize))))))
    


;; A SerialBoard is
;; (List [Listof SerialPlayer]
;;       [Listof SerialRegion])

;; A BoardRep is a [Listof (list Number [Listof BPosn])]

(define SCALE 50)

(define (number->color n)
  (if (= n 1) "red" "blue"))

(define (draw-region r scn)
  (match r
    [(list player-num (list (list x y) ...))
     (foldl           
      (λ (x y scn)
        (place-image (square SCALE 'solid (number->color player-num))
                     (+ (* x SCALE) (* 1/2 SCALE))
                     (+ (* y SCALE) (* 1/2 SCALE))
                     scn))
      scn
      x
      y)]))

(define-class dumb-player
  (fields sboard board)
  
  (define/public (register) LOCALHOST)
  
  (define/public (on-key ke)
    (cond [(key=? ke "d") (make-package this 'done)]
          [else this]))
  
  (define/public (to-draw) 
    (foldl draw-region (empty-scene (* SCALE WIDTH) (* SCALE HEIGHT)) (board)))
  
  (define/public (on-receive msg)
    (match msg
      [(list 'start n sb b) (dumb-player sb b)]
      [_ this])))
    

(define (go)
  (launch-many-worlds (universe (start% 2))
                      (big-bang (dumb-player false empty))
                      ;(big-bang (dumb-player))
                      (big-bang (dumb-player false empty))))
