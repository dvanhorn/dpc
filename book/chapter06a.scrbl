#lang scribble/manual
@(require "../web/utils.rkt"
          (for-label (only-in lang/htdp-intermediate-lambda define-struct ...))
          (for-label (except-in class/1 define-struct ... length))
	  (for-label 2htdp/image)
	  (for-label class/universe))

@title[#:tag "chapter:functions"]{Functions}

Functions were a useful abstraction mechanism.

@#reader scribble/comment-reader
(racketblock
  ;; An ISequence implements 
  ;; - term : Natural -> Number
  ;;   compute the i^th element of this sequence

  (define-class even%
    ;; term : Natural -> Natural
    ;; compute the i^th even number
    (check-expect ((even%) #,(racketidfont ".") term 7) 14)
    (define/public (term i)
      (* i 2)))

  (define-class odd%
    ;; term : Natural -> Natural
    ;; compute the i^th odd number
    (check-expect ((odd%) #,(racketidfont ".") term 7) 15)
    (define/public (term i)
      (add1 (* i 2))))

  (define-class even-series%
    ;; term : Natural -> Natural
    ;; sum up the first n even numbers
    (check-expect ((even-series%) #,(racketidfont ".") term 10) 90)
    (define/public (term n)
      (cond [(zero? n) 0]
            [else (+ ((even%) #,(racketidfont ".") term (sub1 n))
		     (term (sub1 n)))])))

  (define-class odd-series%
    ;; term : Natural -> Natural
    ;; sum up the first n odd numbers
    (check-expect ((odd-series%) #,(racketidfont ".") term 10) 100)
    (define/public (term n)
      (cond [(zero? n) 0]
            [else (+ ((odd%) #,(racketidfont ".") term (sub1 n))
		     (term (sub1 n)))])))
)

We can now apply the process for designing abstractions with 
functions-as-values from
@link["http://htdp.org/2003-09-26/Book/curriculum-Z-H-28.html#node_sec_22.2"]{
@emph{HtDP} Section 22.2}
adapted for functions-as-objects.

@#reader scribble/comment-reader
(racketblock  
  (define-class series%
    ;; Σ : ISequence -> ISequence
    ;; Make sequence summing up first n numbers of sequence
    (check-expect ((series%) #,(racketidfont ".") Σ (even%) #,(racketidfont ".") term 10) 90)
    (check-expect ((series%) #,(racketidfont ".") Σ (odd%) #,(racketidfont ".") term 10) 100)
    (define/public (Σ seq)
      (local [(define-class s%
                (define/public (term n)
		  (cond [(zero? n) 0]
			[else (+ (seq #,(racketidfont ".") term (sub1 n))
				 (term (sub1 n)))])))]
        (s%)))))





