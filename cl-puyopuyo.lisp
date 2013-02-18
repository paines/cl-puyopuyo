;;TODO:
;;get rid of bordeaux-threads by counting ticks / time in sdl event handler idling loop

(defpackage :cl-puyopuyo
  (:use :common-lisp :quicklisp )
  (:export #:cl-puyopuyo))

(in-package :cl-puyopuyo)

(ql:quickload "lispbuilder-sdl")
(ql:quickload "bordeaux-threads")  

(defvar *puyoW* 32)
(defvar *puyoH* 32)
(defvar *fieldW* 6)
(defvar *fieldH* 12)
(defvar *maxCols* 4)

(defparameter *field* (make-array (* *fieldW* *fieldH*)))
(defparameter *solveField* (make-array (* *fieldW* *fieldH*)))

;is this sufficient to generate random colors/numbers ?
(setf *random-state* (make-random-state t))

(defvar *state* nil)
(setf *state* 'unpause)

(defvar *run* nil)
(setf *run* 1)
(defparameter *puyos* (make-list 0))

;;a puyo consists of col 0-3 (blue, red, green and yellow), a state (dropping or landed) and a position
(defstruct puyo
  x
  y
  col
  state) ;;dropping / landed

;;we need two stones, 1st and 2nd
;;these need to be global, so that the updatePosition thread can access the stones
(defvar *first* nil)
(defvar *second* nil)
;(setf *first*  (make-puyo :x 2 :y 0 :col (random *maxCols*) :state 'dropping))
;(setf *second* (make-puyo :x 3 :y 0 :col (random *maxCols*) :state 'dropping))

(setf *first*  (make-puyo :x 3 :y 0 :col 1 :state 'dropping))
(setf *second* (make-puyo :x 3 :y 1 :col 2 :state 'dropping))

(push *first* *puyos*)
(push *second* *puyos*)

;;sdl
(lispbuilder-sdl:init-video)

;;the puyo sprites are 32x32 pixels, so the playfield is 6*32 wide  and 12*32 heigh
(lispbuilder-sdl:window  (* *puyoW* *fieldW*) (* *puyoH* *fieldH*) )
(lispbuilder-sdl:init-subsystems lispbuilder-sdl:sdl-init-timer)
(lispbuilder-sdl:clear-display lispbuilder-sdl:*white*)

;sdl surface pictures for blitting
(defparameter *blue* (lispbuilder-sdl:load-image "puyo_blue.png"))
(defparameter *red* (lispbuilder-sdl:load-image "puyo_red.png"))
(defparameter *green* (lispbuilder-sdl:load-image "puyo_green.png"))
(defparameter *yellow* (lispbuilder-sdl:load-image "puyo_yellow.png"))
(defparameter *gameover* (lispbuilder-sdl:load-image "gameover.png"))
(defparameter *paused* (lispbuilder-sdl:load-image "paused.png"))

(defun clearField ()
  (loop for y from 0 to (- *fieldH* 1)  do
       (loop for x from 0 to (- *fieldW* 1) do	    
	    (setf (aref *field* (getOffset x y)) -1))))


(defun printField ()
  (loop for y from 0 to (- *fieldH* 1) do  
     (loop for x from 0 to (- *fieldW* 1) do	    
	  (format t "~D " (aref *field* (getOffset x y))))
       (format t "~%")))
       
(defun countElements (f elem)
  (print "countElements:")
  (print f)
  (let ((n 0))
    (dotimes (counter (length f))
      (if (= (aref f counter) elem)
	  (setf n (+ n 1))))
    n))

(defun copyField (src dst)
  (loop for y from 0 to (- *fieldH* 1)  do
       (loop for x from 0 to (- *fieldW* 1) do	    
	    (setf (aref dst (getOffset x y)) (aref src (getOffset x y))))))


(defun drawField(f)
  ;;i guess there is a sneaky lispy way to cut this down somehow
  (loop for y from 0 to (- *fieldH* 1)  do
       (loop for x from 0 to (- *fieldW* 1) do	    
	    (let ((col (aref f (getOffset x y))))
	      (case col
		(0 (lispbuilder-sdl:draw-surface-at-* *blue* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*))
		(1 (lispbuilder-sdl:draw-surface-at-* *red* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*))
		(2 (lispbuilder-sdl:draw-surface-at-* *green* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*))
		(3 (lispbuilder-sdl:draw-surface-at-* *yellow* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*)))))))

(defun moveToLeft (f s)
  (setf *state* 'pause)
  (let (
	(fx (slot-value f 'x))
	(fy (slot-value f 'y))
	(sy (slot-value s 'y))
	(sx (slot-value s 'x)))
    (cond 
      ;;stones lay on each other
      ((= fx sx)
       (progn
	 (format t "~%moveToLeft cond1")
	 (if (and (= (aref *field* (getoffset (- fx 1) fy)) -1) (>= (- fx 1) 0)
		  (= (aref *field* (getoffset (- sx 1) sy)) -1) (>= (- sx 1) 0))
	     (progn
	       (setf (slot-value f 'x) (- fx 1))
	       (setf (slot-value s 'x) (- sx 1))))))
      ;;s is left from f
      ((< sx fx)
       (progn
	 (format t "~%moveToLeft cond2")
	 (if (and (= (aref *field* (getoffset (- sx 1) sy)) -1) (>= (- sx 1) 0))
	     (progn
	       (setf (slot-value s 'x) (- sx 1))
	       (setf (slot-value f 'x) (- fx 1))))))
      ;;f is left from s
      ((< fx sx)
       (progn
	 (format t "~%moveToLeft cond3")
	 (if (and (= (aref *field* (getoffset (- fx 1) fy)) -1) (>= (- fx 1) 0))
	     (progn
	       (setf (slot-value f 'x) (- fx 1))	       
	       (setf (slot-value s 'x) (- sx 1))))))))
    (setf *state* 'unpause))

(defun moveToRight (f s)
  (setf *state* 'pause)
  (let ((fx (slot-value f 'x))
	(fy (slot-value f 'y))
	(sy (slot-value s 'y))
	(sx (slot-value s 'x)))
    (format t "~%moveToRight::fx=~D fy=~D sx=~D sy=~D" fx fy sx sy)
    (cond 
      ;;stones lay on each other
      ((= fx sx)
       (progn
	 (format t "~%moveToLeft cond1")
	 (if (and
	      (= (aref *field* (getoffset (+ fx 1) fy)) -1) (< (+ fx 1 ) *fieldW*)
	      (= (aref *field* (getoffset (+ sx 1) sy)) -1) (< (+ sx 1 ) *fieldW*))
	     (progn
	       (setf (slot-value f 'x) (+ fx 1))
	       (setf (slot-value s 'x) (+ sx 1))))))
      ;;fx is left from sx
      ((< fx sx)
       (progn
	 (if (and (= (aref *field* (getoffset (+ sx 1) sy)) -1) (< (+ sx 1) *fieldW*))
	     (progn
	       (setf (slot-value s 'x) (+ sx 1)) 
	       (setf (slot-value f 'x) (+ fx 1))))))	       
      ;;sx is left from sx
      ((< sx fx)
       (progn
	 (if (and (= (aref *field* (getoffset (+ fx 1) fy)) -1) (< (+ fx 1) *fieldW*))
	     (progn
	       (setf (slot-value f 'x) (+ fx 1))
	       (setf (slot-value s 'x) (+ sx 1))))))))
  (setf *state* 'unpause))


(defun rotateStones (f s)
;  (setf *state* 'pause)
  (let ((fx (slot-value f 'x))
	(fy (slot-value f 'y))
	(sy (slot-value s 'y))
	(sx (slot-value s 'x)))
    (format t "~%fx=~D fy=~D sx=~D sy=~D" fx fy sx sy)
    (cond
      ((< fx sx)
       (progn
	 (format t "~%1st")
	 (setf (slot-value f 'y) (- sy 1))
	 (setf (slot-value f 'x) sx)))
      ((< fy sy)(= fx sx)
	(progn
	  (format t "~%2nd")
	  (if (= fx 0)
	      (progn
		(print "sonderfall")
		(setf (slot-value f 'x) (+ fx 1))
		(setf (slot-value f 'y) (+ fy 1)))
	      (progn
		(print "standardfall")
		(setf (slot-value s 'y) (- sy 1))
		(setf (slot-value s 'x) (- sx 1))))))
      ((> fx sx) (= fy sy)
       (progn
	 (format t "~%3rd")
	 (setf (slot-value s 'x) (+ sx 1))
	 (setf (slot-value s 'y) (- sy 1))))
      ((> fy sy)(= fx sx)
	(progn
	  (format t "~%4th")
	  (if (= fx 0)
	      (progn
		(print "sonderfall")
		(setf (slot-value s 'x) (+ sx 1))
		(setf (slot-value s 'y) (+ sy 1)))
	      (progn
		(print "standardfall")
		(setf (slot-value s 'y) (+ sy 1))
		(setf (slot-value f 'x) (- sx 1))))))))
  (format t "~%end rotate stone")
  (setf *state* 'unpause))

(defun getOffset (x y)
  (+ (* y *fieldW*) x))

;(defun dropPuyos (puyos)
(defun dropPuyos ()
  (print "Function to perform dropping of all puyos")
  (setf *state* 'computing)
  (setq *puyos* (sort *puyos* #'> :key #'(lambda (x) (slot-value x 'y))))
  
  (if (eq (length *puyos*) 0)
      (progn
	(setf *state* 'newPuyos)
	nil))

;;  (printField)

  (loop for p in *puyos* do
       (if (eq (slot-value p 'state) 'dropping)
	   (if (and (< (+ (slot-value p 'y) 1) *fieldH*)
		    (= (aref *field* (getOffset (slot-value p 'x) (+ (slot-value p 'y) 1))) -1))
	       (progn
		 (print "dropping")
		 (setf (slot-value p 'y) (+ (slot-value p 'y) 1))
		 (puyosToField *puyos* *field*))
	       
	       (progn

		 (let ((x (slot-value p 'x))
		       (y (slot-value p 'y))
		       (col (slot-value p 'col)))
		   (format t "~%not dropping x=~D y=~D col=~D" x y col)
		   (setf (aref *field* (getOffset x y)) col)
		   (setf (slot-value p 'state) 'landed)
		   (puyosToField *puyos* *field*)
		   ;(push p puyos)
		   nil)))))
  (setf *state* 'unpause))



(defun backtrack (f x y col)
  (if (and (>= x 0) (>= y 0) (< x *fieldW*) (< y *fieldH*))
      (if (= (aref f (getOffset x y)) col)
	  (progn
	    (setf (aref f (getOffset x y)) 255)
	    (if (or
		 (backtrack f (+ x 1) y col)
		 (backtrack f x (+ y 1) col)
		 (backtrack f (- x 1) y col)
		 (backtrack f x (- y 1) col)
		 nil)
		nil))
	  nil)))

(defun sweeping ()
  (print "sweeping!")
  
  (loop for y from 0 to (- *fieldH* 1)  do
       (loop for x from 0 to (- *fieldW* 1) do	    
	    (if (= (aref *solveField* (getOffset x y)) 255)
		(setf (aref *field* (getOffset x y)) -1))))

  (loop for p in *puyos* do
       (pop *puyos*))

  (fieldToPuyos)
  
  (print *field*)

  (print "length of puyos:")
  (print (length *puyos*))

  (loop for p in *puyos* do
       (setf (slot-value p 'state) 'dropping))
  
;  (loop until (dropPuyos *puyos*))
  (loop until (dropPuyos))
  
 
  (print "in sweeping:")
  (print *puyos*)
      
;;  (printField)
  
  (setf *state* 'reset))

(defun updatePosition ()
  "threaded function which will update pos each second"
  (loop do	    
       (print "update position")
       (print "state:")
       (print *state*)
       (cond

	 ((eq *state* 'reset)
	  (print "resetting backtracking")
	  (setf *state* 'backtrack))

	 ((eq *state* 'unpause)
	   (progn
;	     (dropPuyos *puyos*)
	     (dropPuyos)
	     (if (and
		  (eq (slot-value *first* 'state) 'landed)
		  (eq (slot-value *second* 'state) 'landed))
		 (progn
		   ;;cl(os)/gc will destroy objects on its own
		   ;;(delete *first*)
		   ;;(delete *second*)
		   (setf *state* 'backtrack)))))

	 ((eq *state* 'computing)
	  (sleep 0.1)
	  )
       
	 ((eq *state* 'backtrack)
	  (loop for y from 0 to (- *fieldH* 1)  do
	       (loop for x from 0 to (- *fieldW* 1) do	    
		    (if (/= (aref *field* (getOffset x y)) 255)
			(progn
			  (copyField *field* *solveField*)
			  (backtrack *solveField* x y (aref *field* (getOffset x y)))
			  (if (>= (countElements *solveField* 255) 4)
			      (progn
				(sweeping)))))))
;				(return))
	  (setf *state* 'newPuyos))

	 
	 ((eq *state* 'newPuyos) 
	  (print "newPuyos:")
	  (print *puyos*)
	  (if (and (/= (aref *field* (getOffset 2 0)) -1) (/= (aref *field* (getOffset 3 0)) -1))
	      (progn		       
		(setf *run* 0)
		(print "GAME OVER"))
	      (progn
		(format t "~%make new puyos")
		;; we need to create new objects. what isthe impact on memory / gc
		(setf *first*  (make-puyo :x 2 :y 0 :col (random *maxCols*) :state 'dropping))
		(setf *second*  (make-puyo :x 3 :y 0 :col (random *maxCols*) :state 'dropping))

		(push *first* *puyos*)
		(push *second* *puyos*)

		(setf *state* 'unpause)))))	 
	 (sleep .3)
	 while(= 1 *run*))
  (format t "~%updatePosition:end"))
	 

(defun fieldToPuyos ()
  "this helper function will take a field->f and fill the lisp of puyos->p. this is only for testing purpose"
  (loop for y from 0 to (- *fieldH* 1)  do
       (loop for x from 0 to (- *fieldW* 1) do	    
	    (if (/= (aref *field* (getOffset x y)) -1)
		(progn
		  (push (make-puyo :x x :y y :col (aref *field* (getOffset x y)) :state 'landed) *puyos*))))))
  
(defun puyosToField (puyos f)
  "this helper function will take a list of puyos->p and place them into a field ->f"
;  (print "puyosToField called")
  (loop for y from 0 to (- *fieldH* 1)  do
       (loop for x from 0 to (- *fieldW* 1) do	    
	    (setf (aref f (getOffset x y)) -1)))
  (loop for p in puyos do
       (setf (aref f (getOffset (slot-value p 'x) (slot-value p 'y))) (slot-value p 'col))))

(defun fileToField (f)
  "this helper function will read in a file called test.txt and construct a playing field->f. this is for testing purposes..."
  (print "fileToField called")
  (let ((in (open "test.txt"))
	(l 0)
	(data 0))    
    (loop for i from 0 to (- (length f) 1) do
	 (setf data (digit-char-p (read-char in)))
	 (if (eq data nil)
	     (setf data -1))
	 (setf (aref f i) data) 
	 (setf l (+ l 1))
	 (if (eq l 6)
	     (progn
	       (read-char in)
	       (setf l 0))))
    (close in)))


;; (defun sweepField ()
;;game-loop
(clearField)
(bordeaux-threads:make-thread #'updatePosition :name "upos")

;test shit

(fileToField *field*)
(fieldToPuyos)

;;(printField)

(lispbuilder-sdl:with-events (:poll)
  (:quit-event () T)
  (:key-down-event (:key key)
		   (when (lispbuilder-sdl:key= key :sdl-key-escape)
		     (lispbuilder-sdl:push-quit-event))
		   (when (lispbuilder-sdl:key= key :sdl-key-p)
		     ;;pause handling
		     (cond ((eq *state* 'pause)
			    (setf *state* 'unpause))
			   ((eq *state* 'unpause)
			    (setf *state* 'pause))))
		   (when (lispbuilder-sdl:key= key :sdl-key-up)
		     (rotateStones *first* *second*)
		     (puyosToField *puyos* *field*))
		   (when (lispbuilder-sdl:key= key :sdl-key-down)
		   ;  (dropPuyo *first*)
		   ;  (dropPuyo *second*))
;		   (dropPuyos *puyos*))
		   (dropPuyos))
		   (when (lispbuilder-sdl:key= key :sdl-key-left)
		     (movetoLeft *first* *second*))
		   (when (lispbuilder-sdl:key= key :sdl-key-right)
		     (moveToRight *first* *second*)))

  (:idle ()
	 (if (eq *run* 1)
	     (progn
	       (lispbuilder-sdl:clear-display lispbuilder-sdl:*white*)
	       (if (eq *state* 'pause)
		   (lispbuilder-sdl:draw-surface-at-* *paused* 0 0 :surface lispbuilder-sdl:*default-display*)
		   (progn
;		     (puyosToField *puyos* *field*)
		     (drawField *field*)))
;		     (drawPuyo *first*)
;		     (drawPuyo *second*)))
	       (lispbuilder-sdl:update-display))
	     (progn
	       (lispbuilder-sdl:draw-surface-at-* *gameover* 0 0 :surface lispbuilder-sdl:*default-display*)
	       (lispbuilder-sdl:update-display)))))

;;we are done. bye bye
(setf *run* 0)
(sleep 1)
(format t "~%we are done...")
(lispbuilder-sdl:quit-sdl)
