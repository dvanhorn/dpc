#lang scribble/manual
@(require "../utils.rkt"
          (for-label (only-in lang/htdp-intermediate-lambda 
                              define-struct
                              or
                              local
			      check-expect
                              <= < * /))
          (for-label (except-in 2htdp/image color))
          (for-label (only-in 2htdp/universe mouse=?))
          (for-label (except-in class2 
	  	     		check-expect
                                define-struct)))

@title[#:tag "soln04"]{2/2: Tron}

This is a solution for the @seclink["assign04"]{Tron}
assignment that was due on 2/2.

@itemlist[#:style 'ordered
  @item{@bold{Distributed Tron}

@filebox["1.rkt"]{
@#reader scribble/comment-reader
(racketmod
  class2
  ;;==========================================================
  ;; Two player distributed Tron

  (require class1/universe)
  (require 2htdp/image)  
  (provide serve play 
	   mt% final% 
	   WAITING MT-SCENE 
	   draw-match outcome?)  

  ;; -> Universe World World
  ;; Play a game locally.
  (define (play-local)
    (launch-many-worlds (serve)
			(play LOCALHOST)
			(play LOCALHOST)))
  
  
  ;;==========================================================
  ;; Shared
  
  ;; A Trail is a (cons Posn [Listof Posn]).
  ;; A Posn is a [List Number Number].
  (define (posn x y) (list x y))
  (define posn-x first)
  (define posn-y second)
  
  (define GRID-WIDTH 60)
  (define GRID-HEIGHT 60)
  
  ;;==========================================================
  ;; Server
  
  ;; A Universe is one of:   Interp:
  ;; - (init%)               - No players
  ;; - (wait% IWorld)        - One player, waiting on second
  ;; - (game% Cycle Cycle)   - two player game
  
  ;; A Cycle is a (cycle% IWorld Dir Trail)
  ;; Interp: owner, direction, and trail.

  (define (serve)
    (universe (init%)))
  
  ;; Universe -> Bundle
  ;; Make a bundle with just the given universe.
  (define (just u)
    (make-bundle u empty empty))  
  
  (define-class init%
    (define/public (tick-rate) 1/2)
    
    ;; IWorld -> Universe
    ;; Wait for second player.
    (check-expect ((init%) #,dot on-new iworld1)
		  (just (wait% iworld1)))
    (define/public (on-new iw)
      (just (wait% iw)))
    
    ;; -> Universe
    ;; Continue to wait.
    (check-expect ((init%) #,dot on-tick) (just (init%)))
    (define/public (on-tick)
      (just this))
    
    ;; IWorld SExpr -> Universe
    ;; Ignore messages, continue to wait.
    (check-expect ((init%) #,dot on-msg iworld1 'hi!)
		  (just (init%)))
    (define/public (on-msg iw msg)
      (just this)))
  
  (define-class wait%
    (fields w)
    
    ;; IWorld -> Universe
    ;; Let the game begin!
    (check-expect ((wait% iworld1) #,dot on-new iworld2)
		  (just (init-game iworld1 iworld2)))
    (define/public (on-new iw)
      (just (init-game (field w) iw)))
    
    ;; -> Universe
    ;; Continue to wait.
    (check-expect ((wait% iworld1) #,dot on-tick)
		  (just (wait% iworld1)))
    (define/public (on-tick)
      (just this))
    
    ;; IWorld SExpr -> Universe
    ;; Ignore messages, continue to wait.
    (check-expect ((wait% iworld1) #,dot on-msg iworld1 'hi!)
		  (just (wait% iworld1)))
    (define/public (on-msg iw msg)
      (just this)))
  
  (define-class game% 
    (fields p1 p2)
    
    ;; IWorld -> Universe
    ;; Ignore new worlds.
    (check-expect ((game% c1 c2) #,dot on-new iworld1)
		  (just (game% c1 c2)))
    (define/public (on-new iw)
      (make-bundle this empty empty))
    
    ;; -> Universe
    ;; Advance this universe one tick.
    (check-expect ((game% c1 c2) #,dot on-tick)
		  (make-bundle ((game% c1 c2) #,dot tick)
			       ((game% c1 c2) #,dot broadcast)
			       empty))
    (check-expect ((game% c1 c2) #,dot tick #,dot on-tick)
		  (make-bundle ((game% c1 c2) #,dot tick)
			       ((game% c1 c2) #,dot tick #,dot end)
			       empty))
    (define/public (on-tick)
      (cond [(game-over?)
	     (make-bundle this (end) empty)]
	    [else
	     (make-bundle (tick) (broadcast) empty)]))
    
    ;; IWorld SExpr -> Universe
    ;; Change direction if appropriate.
    (check-expect ((game% c1 c2) #,dot on-msg iworld1 "up")
		  (just ((game% c1 c2) #,dot dir iworld1 "up")))
    (check-expect ((game% c1 c2) #,dot on-msg iworld1 "fred")
		  (just (game% c1 c2)))
    (define/public (on-msg iw msg)
      (cond [(dir? msg)
	     (just (dir iw msg))]
	    [else
	     (just this)]))
    
    ;; -> Game
    ;; Advance this game one tick.
    (check-expect ((game% c1 c2) #,dot tick)
		  (game% (c1 #,dot tick) (c2 #,dot tick)))
    (define/public (tick)
      (game% ((field p1) #,dot tick)
	     ((field p2) #,dot tick)))
    
    ;; IWorld Dir -> Game
    ;; Change the direction of the given iworld's cycle.
    (check-expect ((game% c1 c2) #,dot dir iworld1 "up")
		  (game% (c1 #,dot dir "up") c2))
    (check-expect ((game% c1 c2) #,dot dir iworld2 "up")
		  (game% c1 (c2 #,dot dir "up")))
    (define/public (dir iw d)
      (cond [(iworld=? iw ((field p1) #,dot owner))
	     (game% ((field p1) #,dot dir d)
		    (field p2))]
	    [(iworld=? iw ((field p2) #,dot owner))
	     (game% (field p1)
		    ((field p2) #,dot dir d))]))
    
    ;; -> [Listof Mail]
    ;; Broadcast state of game to players.
    (check-expect ((game% c1 c2) #,dot broadcast)
		  (list (make-mail iworld1 '(((0 0))
					     ((1 0))))
			(make-mail iworld2 '(((1 0))
					     ((0 0))))))
    (define/public (broadcast)
      (list (make-mail ((field p1) #,dot owner)
		       (list ((field p1) #,dot trail)
			     ((field p2) #,dot trail)))
	    (make-mail ((field p2) #,dot owner)
		       (list ((field p2) #,dot trail)
			     ((field p1) #,dot trail)))))
    
    ;; -> Boolean
    ;; Is this game over?
    (check-expect ((game% c1 c2) #,dot game-over?) false)
    (check-expect ((game% c1 c2) #,dot tick #,dot game-over?) true)
    (define/public (game-over?)
      (local [(define t1 ((field p1) #,dot trail))
	      (define t2 ((field p2) #,dot trail))]
        (or (die? t1 t2)
	    (die? t2 t1))))
    
    ;; -> [Listof Mail]
    ;; Construct mail notifying win, lose, or draw.
    (check-expect ((game% c1 c2) #,dot tick #,dot end)
		  (append ((game% c1 c2) #,dot tick #,dot broadcast)
			  ((game% c1 c2) #,dot tick #,dot end-draw)))
    (define/public (end)
      (append (broadcast)
	      (cond [(die? ((field p1) #,dot trail)
			   ((field p2) #,dot trail))
		     (cond [(die? ((field p2) #,dot trail)
				  ((field p1) #,dot trail))
			    (end-draw)]
			   [else
			    (end-p2-win)])]
		    [else 
		     (end-p1-win)])))
    
    ;; -> [Listof Mail]
    ;; Construct mail notifying draw.
    (check-expect ((game% c1 c2) #,dot end-draw)
		  (list (c1 #,dot mail 'draw)
			(c2 #,dot mail 'draw)))
    (define/public (end-draw)
      (list ((field p1) #,dot mail 'draw)
	    ((field p2) #,dot mail 'draw)))
    
    ;; -> [Listof Mail]
    ;; Construct mail notifying P1 win, P2 loss.
    (check-expect ((game% c1 c2) #,dot end-p1-win)
		  (list (c1 #,dot mail 'win)
			(c2 #,dot mail 'lose)))
    (define/public (end-p1-win)
      (list ((field p1) #,dot mail 'win)
	    ((field p2) #,dot mail 'lose)))
    
    ;; -> [Listof Mail]
    ;; Construct mail notifying P2 win, P1 loss.
    (check-expect ((game% c1 c2) #,dot end-p2-win)
		  (list (c1 #,dot mail 'lose)
			(c2 #,dot mail 'win)))
    (define/public (end-p2-win)
      (list ((field p1) #,dot mail 'lose)
	    ((field p2) #,dot mail 'win))))
  
  (define-class cycle%
    (fields owner direction trail)
    
    ;; Cycle -> Cycle
    ;; Advance this cycle one tick.
    (check-expect (c1 #,dot tick)
		  (cycle% iworld1 "right" '((1 0) (0 0))))
    (define/public (tick)
      (cycle% (field owner)
	      (field direction)
	      (trail-grow (field trail) 
			  (field direction))))
    
    ;; Msg -> Mail
    ;; Construct a mail message to the owner of this cycle.
    (check-expect (c1 #,dot mail "hi!")
		  (make-mail iworld1 "hi!"))
    (define/public (mail msg)
      (make-mail (field owner) msg))
    
    ;; Dir -> Cycle
    ;; Change the direction of this cycle.
    (check-expect (c1 #,dot dir "up")
		  (cycle% iworld1 "up" '((0 0))))
    (define/public (dir d)
      (cycle% (field owner) d (field trail))))
  
  ;; For testing
  (define c1 (cycle% iworld1 "right" (list (posn 0 0))))
  (define c2 (cycle% iworld2 "left"  (list (posn 1 0))))
  
  
  ;; A Dir is one of: "up", "down", "left", "right"
  
  ;; Any -> Boolean
  ;; Is the given value a direction?
  (check-expect (dir? "a") false)
  (check-expect (dir? "right") true)
  (define (dir? x)
    (member x '("up" "down" "left" "right")))
  
  ;; Trail Dir -> Trail
  ;; Grow this light trail in the given direction.
  (check-expect (trail-grow '((0 0)) "right") '((1 0) (0 0)))
  (check-expect (trail-grow '((0 0)) "up")    '((0 1) (0 0)))
  (check-expect (trail-grow '((0 1)) "down")  '((0 0) (0 1)))
  (check-expect (trail-grow '((1 0)) "left")  '((0 0) (1 0)))
  (define (trail-grow t d)
    (cons (cond [(string=? d "up") 
		 (posn (posn-x (first t))
		       (add1 (posn-y (first t))))]
		[(string=? d "down")
		 (posn (posn-x (first t))
		       (sub1 (posn-y (first t))))]
		[(string=? d "left")
		 (posn (sub1 (posn-x (first t)))
		       (posn-y (first t)))]
		[(string=? d "right")
		 (posn (add1 (posn-x (first t)))
		       (posn-y (first t)))])
	  t))
  
  ;; Trail Trail -> Boolean
  ;; Does t1 die by going out of bounds or hitting t2?
  (check-expect (die? '((0 0)) '((1 0))) false)
  (check-expect (die? '((0 0)) '((0 0))) true)
  (check-expect (die? '((-1 0)) '((1 0))) true)
  (define (die? t1 t2)
    (or (out-of-bounds? t1)
	(hit-trail? t1 t2)))
  
  ;; Trail -> Boolean
  ;; Is this trail out of bounds?
  (check-expect (out-of-bounds? '((0 0))) false)
  (check-expect (out-of-bounds? '((-1 0))) true)
  (check-expect (out-of-bounds? '((0 -1))) true)
  (define (out-of-bounds? t)
    (local [(define hd (first t))]
      (or (< (posn-x hd) 0)
	  (> (posn-x hd) GRID-WIDTH)
	  (< (posn-y hd) 0)
	  (> (posn-y hd) GRID-HEIGHT))))
  
  ;; Trail Trail -> Boolean
  ;; Does the first trail hit the second?
  (check-expect (hit-trail? '((0 0)) '((1 0))) false)
  (check-expect (hit-trail? '((0 0)) '((0 0))) true)
  (define (hit-trail? t1 t2)
    (member (first t1)
	    (append (rest t1) t2)))
  
  ;; IWorld IWorld -> Game
  ;; Construct an initial game between the two given worlds.
  (define (init-game iw1 iw2)
    (game% (cycle% iw1 
		   "right" 
		   (list (posn (quotient GRID-WIDTH 3)
			       (quotient GRID-HEIGHT 2))))
	   (cycle% iw2 
		   "left"
		   (list (posn (* 2 (quotient GRID-WIDTH 3))
			       (quotient GRID-HEIGHT 2))))))
  

  ;;==========================================================
  ;; Client
  
  ;; A World is one of:
  ;; - (mt%)
  ;; - (match% (list Trail Trail))
  ;; - (final% Outcome (list Trail Trail))
  
  ;; An Outcome is one of: 'win, 'lose, 'draw
  
  ;; IP -> World
  (define (play ip)
    (big-bang (mt% ip)))
  
  (define GRID-SIZE 10)
  (define WIDTH (* GRID-WIDTH GRID-SIZE))
  (define HEIGHT (* GRID-HEIGHT GRID-SIZE))
  
  (define MT-SCENE (empty-scene WIDTH HEIGHT))
  (define WAITING 
    (overlay (text "Waiting for Player 2" 20 "black")
	     MT-SCENE))
  
  ;; Any -> Boolean
  (check-expect (outcome? "fred") false)
  (check-expect (outcome? 'draw) true)
  (define (outcome? w)
    (member w '(win lose draw)))
  
  (define-class mt%
    (fields ip)
    
    ;; -> String
    ;; IP address where world should register.
    (check-expect ((mt% LOCALHOST) #,dot register) LOCALHOST)
    (define/public (register) (field ip))
    
    ;; -> Scene
    ;; Render this waiting world as a scene.
    (check-expect ((mt% LOCALHOST) #,dot to-draw) WAITING)
    (define/public (to-draw) WAITING)
    
    ;; KeyEvent -> World
    ;; Ignore key events.
    (check-expect ((mt% LOCALHOST) #,dot on-key "up")
		  (mt% LOCALHOST))
    (define/public (on-key ke) this)
    
    ;; (list Trail Trail) -> World
    ;; Start playing with given trails.
    (check-expect ((mt% LOCALHOST) #,dot on-receive '(() ()))
		  (match% '(() ())))
    (define/public (on-receive msg)   
      (match% msg)))
  
  (define-class match%
    (fields m)
    
    ;; -> Scene
    ;; Render this match as a scene.
    (define/public (to-draw)
      (draw-match (field m)))
    
    ;; KeyEvent -> World
    ;; Handle directional keys by sending to server.
    (check-expect ((match% '(() ())) #,dot on-key "up")
		  (make-package (match% '(() ())) "up"))
    (check-expect ((match% '(() ())) #,dot on-key "a")
		  (match% '(() ())))
    (define/public (on-key ke)
      (cond [(dir? ke) (make-package this ke)]
	    [else this]))
    
    ;; (U Outcome (list Trail Trail)) -> World
    ;; Handle an outcome or new state.
    (check-expect ((match% '(() ())) #,dot on-receive 'draw)
		  (final% 'draw '(() ())))
    (check-expect ((match% '(() ())) #,dot on-receive '((()) ()))
		  (match% '((()) ())))                
    (define/public (on-receive msg)
      (cond [(outcome? msg) (final% msg (field m))]
	    [else (match% msg)])))
  
  (define-class final%
    (fields outcome last)
    
    ;; -> Scene
    ;; Render this final outcome as a scene.
    (check-expect ((final% 'draw '(() ())) #,dot to-draw)
		  (overlay (text "TIE" 40 "black")
			   MT-SCENE))
    (define/public (to-draw)
      (overlay 
       (text (cond
	      [(symbol=? (field outcome) 'win) "You Win!"]
	      [(symbol=? (field outcome) 'lose) "Loser!"]
	      [(symbol=? (field outcome) 'draw) "TIE"])
	     40
	     "black")
       (draw-match (field last))))
    
    ;; KeyEvent -> World
    ;; Ignore key events.
    (check-expect ((final% 'draw '(() ())) #,dot on-key "down")
		  (final% 'draw '(() ())))                
    (define/public (on-key ke)
      this)
    
    ;; SExpr -> World
    ;; Ignore messages.
    (check-expect ((final% 'draw '(() ())) #,dot on-receive "hi!")
		  (final% 'draw '(() ())))
    (define/public (on-receive msg)
      this))
  
  
  ;; (list Trail Trail) -> Scene
  ;; Render the given trails as a scene.
  (check-expect (draw-match '(() ())) MT-SCENE)
  (check-expect (draw-match '(((0 0)) ()))
		(place-image (square GRID-SIZE "solid" "orange")
			     (* 1/2 GRID-SIZE)
			     (- HEIGHT (* 1/2 GRID-SIZE))
			     MT-SCENE))
  (check-expect (draw-match '(() ((0 0))))
		(place-image (square GRID-SIZE "solid" "blue")
			     (* 1/2 GRID-SIZE)
			     (- HEIGHT (* 1/2 GRID-SIZE))
			     MT-SCENE))
  (define (draw-match m)
    (local [(define (draw-player c trail scn)
	      (foldl (λ (p scn)
			(place-image 
			 (square GRID-SIZE "solid" c)
			 (+ (* (posn-x p) GRID-SIZE)
			    (* 1/2 GRID-SIZE))
			 (- HEIGHT
			    (+ (* (posn-y p) GRID-SIZE)
			       (* 1/2 GRID-SIZE)))
			 scn))
		     scn
		     trail))]
      (draw-player "blue"
		   (second m)
		   (draw-player "orange" 
				(first m) 
				MT-SCENE))))
)}
}

  @item{@bold{Computer Tron}

@filebox["2.rkt"]{
@#reader scribble/comment-reader
(racketmod
  class2
  ;; =========================================================
  ;; Computer Tron
  
  (require class1/universe)
  (require 2htdp/image)
  (require "1.rkt")
  
  ;; =========================================================
  ;; Computer
  
  ;; A Computer is one of:
  ;; - (cmt%)
  ;; - (cmatch% (list Trail Trail))
  ;; - (final% Outcome (list Trail Trail))
  
  ;; An Outcome is one of: 'win, 'lose, 'draw
  
  ;; This computer player drives around in a square.
  
  ;; -> Universe World Computer
  ;; Play a computer player locally.
  (define (play-local-computer)
    (launch-many-worlds (serve)
			(play LOCALHOST)
			(big-bang (cmt% LOCALHOST))))
  
  (define STEPS-UNTIL-TURN 10)
  
  (define-class cmt%
    (fields ip)
    
    ;; -> String
    ;; IP address where world should register.
    (check-expect ((cmt% LOCALHOST) #,dot register) LOCALHOST)
    (define/public (register) (field ip))
    
    ;; -> Scene
    ;; Render this waiting world as a scene.
    (check-expect ((cmt% LOCALHOST) #,dot to-draw) WAITING)
    (define/public (to-draw) WAITING)
    
    ;; (list Trail Trail) -> Package
    ;; Start playing with given trails.
    (check-expect ((cmt% LOCALHOST) #,dot on-receive '(() ()))
		  (make-package 
		   (cmatch% '(() ()) STEPS-UNTIL-TURN "left")
		   "down"))
    (define/public (on-receive msg)   
      (make-package (cmatch% msg 
			     STEPS-UNTIL-TURN 
			     (next-dir "down"))
		    "down")))
  
  (define-class cmatch%
    (fields m steps dir)
    
    ;; -> Scene
    ;; Render this match as a scene.
    (define/public (to-draw)
      (place-image (text "Computer" 30 "black")
		   100 50
		   (draw-match (field m))))
    
    ;; (U Outcome (list Trail Trail)) -> (U World Package)
    ;; Handle an outcome or new state.
    (check-expect ((cmatch% '(() ()) 10 "down") #,dot on-receive 'draw)
		  (final% 'draw '(() ())))
    (check-expect ((cmatch% '(() ()) 10 "down") #,dot on-receive '((()) ()))
		  (cmatch% '((()) ()) 9 "down"))
    (define/public (on-receive msg)
      (cond [(outcome? msg) (final% msg (field m))]
	    [(zero? (field steps))
	     (make-package (cmatch% msg 
				    STEPS-UNTIL-TURN 
				    (next-dir (field dir)))
			   (field dir))]
	    [else (cmatch% msg 
			   (sub1 (field steps))
			   (field dir))])))
  
  ;; Dir -> Dir
  ;; The next direction in a square.
  (define (next-dir d)
    (cond [(string=? "down" d) "left"]
	  [(string=? "left" d) "up"]
	  [(string=? "up" d) "right"]
	  [(string=? "right" d) "down"]))
  )
}
}
]