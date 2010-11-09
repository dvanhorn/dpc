#lang scribble/manual
@(require (only-in racket first rest make-list))

@title{Syllabus}

@(define (number->string2 n)
   (let ((r (number->string n)))
     (cond [(= (string-length r) 1)
            (string-append "0" r)]
           [else r])))

@(define (syllabus-row i reading)
   (list (number->string i)
         (itemlist (item (secref (string-append "lec" (number->string2 (- (* 2 i) 2)))))
                   (item (secref (string-append "lec" (number->string2 (- (* 2 i) 1))))))
         reading
         (secref (string-append "lab" (number->string2 i)))
         (secref (string-append "assign" (number->string2 i)))))

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
