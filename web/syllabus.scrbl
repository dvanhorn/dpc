#lang scribble/manual
@(require (only-in racket first rest make-list)
	  "utils.rkt")

@title{Syllabus}

@(define (number->string2 n)
   (let ((r (number->string n)))
     (cond [(= (string-length r) 1)
            (string-append "0" r)]
           [else r])))

@(define WEEK-REVEALED (if-internal 15 1))

@(define assign-dates
   (list "1/12"
	 "1/19"
	 "1/26"
	 "2/2"
	 "2/9"
	 "2/18 (Non-standard day)"
	 "2/25 (Non-standard day)"
	 "3/2 Spring break"
	 "3/9"
	 "3/16"
	 "3/23"
	 "3/30"
	 "4/6"
	 "4/13"
	 "4/20"))

@(define lab-dates
   (list "1/10"
	 "1/17 MLK"
	 "1/24"
	 "1/31"
	 "2/7"
	 "2/14"
	 "2/21 Presidents"
	 "2/28 Spring break"
	 "3/7"
	 "3/14"
	 "3/21"
	 "3/28"
	 "4/4"
	 "4/11"
	 "4/18 Patriots"))
	 

@(define (syllabus-row i reading)
   (list (number->string i)
         (itemlist (item (secref (string-append "lec" (number->string2 (- (* 2 i) 2)))))
                   (item (secref (string-append "lec" (number->string2 (- (* 2 i) 1))))))
         reading
         (if (<= i WEEK-REVEALED)
	     (secref (string-append "lab" (number->string2 i)))
	     (list-ref lab-dates (sub1 i)))
	 (if (<= i WEEK-REVEALED)
	     (secref (string-append "assign" (number->string2 i)))
	     (list-ref assign-dates (sub1 i)))))

@(define reading-list
   (append (list "HtDC: Ch. 1, 2, 10."
		 "HtDC: Ch. 3, 4, 5."
		 "HtDC: Ch. 6, 11, 12."
		 "Universe docs.")
	   (make-list 11 "??")))	 

@(tabular (apply list 
                 (list "" "Lectures " "Readings " "Lab " "Assignment")
                 (list "1"    
		       (itemlist (item (secref "lec01")))
                       (first reading-list)
		       (secref "lab01")
		       (secref "assign01"))
                 (for/list ([i (in-range 2 16)]
                            [r (in-list (rest reading-list))])
                   (syllabus-row i r))))
