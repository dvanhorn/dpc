#lang racket


(define x (sort #:cache-keys? #t #:key (λ _ (random)) (file->lines "student-list") <))

(define one (first x))
(define others (rest x))

(define pairs (map list (take others 17) 
                   (drop  others 17)))

(define (get-username s) (second (regexp-match ".*?([a-z0-9]+)$" s)))

(define pairs*
  (cons (cons one (first pairs))
        (rest pairs)))

(define all-users (map (λ (l) (map get-username l)) pairs*))

;; the first real partnership assignment.  Use to rerun if neccessary.
(define all-users1
  '(("mcoppola" "mcarroll" "naraghi")
    ("lindeman" "robbyk")
    ("dcorbett" "chiner")
    ("laplante" "rpless")
    ("demaior" "kardon")
    ("mmitlin" "forrest")
    ("morehd" "gierb")
    ("hloople" "petey3")
    ("pfurtado" "dmsilva")
    ("fungi" "gressen")
    ("jgramm" "florence")
    ("hfried7" "ngoldman")
    ("jccoates" "markefus")
    ("tswartz" "schwarta")
    ("wconlon" "aldwin")
    ("ekelly" "maxgoren")
    ("fnimick" "wjhsie")))

(define (pad i)
  (define s (number->string i))
  (cond [(< i 10) (string-append "00" s)]
        [(< i 100) (string-append "0" s)]
        [else (string-append "" s)]))

(for ([members all-users]
      [i (in-naturals 1)])
  (printf "[cs2510hspring2011:/pair~a]\n" (pad i))
  (for ([m members])
    (printf "~a = rw\n" m))
  (printf "\n"))

(printf "for the blog\n")

(for ([members all-users]
      [i (in-naturals 1)])
  (printf "pair ~a: " (pad i))
  (for ([m (add-between members ", ")]) (display m))
  (newline))
