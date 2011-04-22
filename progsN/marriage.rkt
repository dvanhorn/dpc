#lang class4
;; A Person implements:
(define-interface person<%>
  [name ; -> String
   ;; The name of this person
   
   intended ; -> Person or #f
   ;; This person's intended significant other, if any.
   
   preferences ; -> [Listof Person]
   ;; People this person would marry, in order of preference.
   
   possibles ; -> [Listof Person]
   ;; People this person has not proposed to, but is still interested in, in
   ;; order of preference.
   
   load-preferences! ; [Listof Person] ->
   ;; Effect: set this person's prefences to the given list.
   
   reset! ; ->
   ;; Effect: Reinitialize this person's intended (#f), possibles (empty), and
   ;;  preferences (empty).
   
   propose! ; -> Boolean
   ;; Effect: Propose to the most desired person not already proposed to.
   ;; Produces true if proposal is accepted, false if rejected.
   
   i-love-you! ; Person -> Boolean
   ;; This message represents a proposal from the given person to this
   ;; person.
   ;; Effect: set the intended to the given person, if accepted.
   ;; Produce true if accepted, false otherwise.
   
   i-changed-my-mind! ; Person ->
   ;; This message represents a break up from the given person to this
   ;; person.
   ;; Effect: set the intended to #f.
   ;; Assume: this person is the given person's intended and vice versa.
   
   i-like-more? ; Person Person -> Boolean
   ;; Does this person like the first person more than the second?
   
   couple? ; Person -> Boolean
   ;; Are this person and the given person engaged to each other?
   ])

(define-class person%
  (implements person<%>)
  (fields name intended preferences possibles)
  
  (constructor (n)
    (fields n false empty empty))
  
  (define/public (load-preferences! ps)
    (begin
      (set-field! possibles ps)
      (set-field! preferences ps)))
  
  (define/public (reset!)
    (begin
      (set-field! preferences empty)
      (set-field! possibles empty)
      (set-field! intended false)))
  
  (define/public (propose!)
    (cond [(empty? (field possibles)) (error "No one to propose to.")]
          [else            
             (local [(define p (first (field possibles)))]
               (begin
                 (set-field! possibles (rest (field possibles)))                 
                 (cond [(send p i-love-you! this)
                        (begin (set-field! intended p)
                               true)]
                       [else false])))]))
                        
  
  (define/public (i-love-you! p)
    (cond [(false? (field intended))
           (begin 
             (set-field! intended p)
             true)]
          [(i-like-more? p (field intended))
           (begin
             ((field intended) . i-changed-my-mind! this)
             (set-field! intended p)
             true)]       
          [else false]))
  
  (define/public (i-changed-my-mind! p)
    (cond [(eq? p (field intended))
           (set-field! intended false)]
          [else
           (error "There is some misunderstanding")]))
  
  (define/public (i-like-more? p1 p2)
    (local [(define (loop ps)
              (cond [(eq? (first ps) p1) true]
                    [(eq? (first ps) p2) false]
                    [else
                     (loop (rest ps))]))]
      (loop (field preferences))))              
  
  (define/public (couple? p)
    (eq? (field intended) p))
  
  (define/public (engaged?)
    (not (false? (field intended)))))
      

;; courtship! : [Listof People] [Listof People] -> [Listof (list String String)]
;; Effect: Marry all of the given proposers and proposees, producing
;; a stable configuration.
;; Produces a list of couple's name, proposer first.
(define (courtship! ms ws)
  (local [(define (loop)
            (cond [(and (andmap (λ (m) (m . engaged?)) ms)
                        (andmap (λ (w) (w . engaged?)) ws))                   
                   (map (λ (m) (list (m . name)
                                     (m . intended . name)))
                        ms)]
                  [else
                   (begin
                     (for-each (λ (m)
                                 (cond [(m . engaged?) 'ok]
                                       [else (m . propose!)]))
                               ms)
                     (loop))]))]
    (loop)))
  
  
  
(define alan (person% "Alan"))
(define bob (person% "Bob"))
(define charles (person% "Chuck"))
(define david (person% "Dave"))
(define ernest (person% "Ernie"))
(define franklin (person% "Frank"))
(define agnes (person% "Agnes"))
(define bertha (person% "Bertha"))
(define carol (person% "Carol"))
(define deborah (person% "Debbie"))
(define ellen (person% "Ellen"))
(define francine (person% "Fran"))

(alan . load-preferences! (list agnes carol francine bertha deborah ellen))
(bob . load-preferences! (list carol francine bertha deborah agnes ellen))
(charles . load-preferences! (list agnes francine carol deborah bertha ellen))
(david . load-preferences! (list francine ellen deborah agnes carol bertha))
(ernest . load-preferences! (list ellen carol francine agnes deborah bertha))
(franklin . load-preferences! (list ellen carol francine bertha agnes deborah))

(agnes . load-preferences! (list charles alan bob david ernest franklin))
(bertha . load-preferences! (list charles alan bob david ernest franklin))
(carol . load-preferences! (list franklin charles bob alan ernest david))
(deborah . load-preferences! (list bob alan charles franklin david ernest))
(ellen . load-preferences! (list franklin charles bob alan ernest david))
(francine . load-preferences! (list alan bob charles david franklin ernest))

(define men (list alan bob charles david ernest franklin))
(define women (list agnes bertha carol deborah ellen francine))

(courtship! men women)