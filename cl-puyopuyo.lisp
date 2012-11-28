(defpackage :cl-puyopuyo
  (:use :common-lisp :quicklisp ))

(in-package :cl-puyopuyo)

(ql:quickload "lispbuilder-sdl")
(ql:quickload "bordeaux-threads")




(format t "~%init...")

(defvar *puyoW* 32)
(defvar *puyoH* 32)
(defvar *fieldW* 6)
(defvar *fieldH* 12)
(defvar *maxCols* 4)

					;(defvar *lastTicks* 0)
(defvar *matchStones* 0)
(setf  *matchStones* 0)


(defparameter *field* (make-array (* *fieldW* *fieldH*)))
(defparameter *solveField* (make-array (* *fieldW* *fieldH*)))
(setf *random-state* (make-random-state t))

(defvar *state* nil)
(setf *state* 'unpause)

(defvar *run* nil)
(setf *run* 1)

(defparameter *coordsList* (make-list 0))

					;a puyo consists of col 0-3 (blue, red, green and yellow and a position
(defstruct puyo
  x
  y
  col)

;;we need two stones, 1st and 2nd
;;these need to be global, so that the updatePosition thread can access the stones
(defvar *first* nil)
(defvar *second* nil)
;;(setf *first*  (make-puyo :x 2 :y 0 :col (random *maxCols*)))
;;(setf *second* (make-puyo :x 3 :y 0 :col (random *maxCols*)))
(setf *first*  (make-puyo :x 2 :y 0 :col 0))
(setf *second* (make-puyo :x 3 :y 0 :col 0))


;;sdl
(lispbuilder-sdl:init-video)

;;the puyo sprites are 32x32 pixels
(lispbuilder-sdl:window  (* *puyoW* *fieldW*) (* *puyoH* *fieldH*) )
(lispbuilder-sdl:init-subsystems lispbuilder-sdl:sdl-init-timer)
(lispbuilder-sdl:clear-display lispbuilder-sdl:*white*)


(defparameter *blue* (lispbuilder-sdl:load-image "puyo_blue.png"))
(defparameter *red* (lispbuilder-sdl:load-image "puyo_red.png"))
(defparameter *green* (lispbuilder-sdl:load-image "puyo_green.png"))
(defparameter *yellow* (lispbuilder-sdl:load-image "puyo_yellow.png"))
(defparameter *gameover* (lispbuilder-sdl:load-image "gameover.png"))

(defun drawPuyo (puyo)
  (let ((x (slot-value puyo 'x))
	(y (slot-value puyo 'y))
	(col (slot-value puyo 'col)))
    (if (and (< x *fieldW*) (< y *fieldH*) (< col 4))
	(case col
	  (0 (lispbuilder-sdl:draw-surface-at-* *blue* (* x 32) (* y 32)  :surface lispbuilder-sdl:*default-display*))
	  (1 (lispbuilder-sdl:draw-surface-at-* *red* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*))
	  (2 (lispbuilder-sdl:draw-surface-at-* *green* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*))
	  (3 (lispbuilder-sdl:draw-surface-at-* *yellow* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*))))))

(defun clearField ()
  (loop for y from 0 to (- *fieldH* 1)  do
       (loop for x from 0 to (- *fieldW* 1) do	    
	    (setf (aref *field* (getOffset x y)) -1))))

(defun copyField (src dst)
  (loop for y from 0 to (- *fieldH* 1)  do
       (loop for x from 0 to (- *fieldW* 1) do	    
	    (setf (aref dst (getOffset x y)) (aref src (getOffset x y))))))


(defun drawField(f)
  (loop for y from 0 to (- *fieldH* 1)  do
       (loop for x from 0 to (- *fieldW* 1) do	    
	    (let ((col (aref f (getOffset x y))))
	      (case col
		(0 (lispbuilder-sdl:draw-surface-at-* *blue* (* x 32) (* y 32)  :surface lispbuilder-sdl:*default-display*))
		(1 (lispbuilder-sdl:draw-surface-at-* *red* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*))
		(2 (lispbuilder-sdl:draw-surface-at-* *green* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*))
		(3 (lispbuilder-sdl:draw-surface-at-* *yellow* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*)))))))


(defun moveToLeft (f s)
  (setf *state* 'pause)
  (let ((fx (slot-value f 'x))
	(fy (slot-value f 'y))
	(sy (slot-value s 'y))
	(sx (slot-value s 'x)))
    (if (and (< fx sx) (> fx 0) (>= sx 0)(= (aref *field* (getOffset (- fx 1) fy)) -1))
	(progn
	  (setf (slot-value f 'x) (- fx 1))
	  (setf (slot-value s 'x) (- sx 1))))
    (if (and (> fx sx) (>= fx 0) (> sx 0)(= (aref *field* (getOffset (- fx 1) fy)) -1))
	(progn
	  (setf (slot-value f 'x) (- fx 1))
	  (setf (slot-value s 'x) (- sx 1))))
    (if (and (= fx sx) (= (aref *field* (getOffset (- sx 1) (- sy 1))) -1) (= (aref *field* (getOffset (- fx 1) (- fy 1))) -1))
	(progn
	  (setf (slot-value f 'x) (- fx 1))
	  (setf (slot-value s 'x) (- fx 1)))))
  (setf *state* 'unpause))

(defun moveToRight (f s)
  (setf *state* 'pause)
  (let ((fx (slot-value f 'x))
	(fy (slot-value f 'y))
	(sy (slot-value s 'y))
	(sx (slot-value s 'x)))
    ;;check if fx<sx, field x-1 is empty and if we are in bounds
    (if (and (< fx sx) (< fx (- *fieldW* 1)) (< sx (- *fieldW* 1))(= (aref *field* (getOffset (+ sx 1) sy)) -1))
	(progn
	  (setf (slot-value s 'x) (+ sx 1))
	  (setf (slot-value f 'x) (+ fx 1))))
    (if (and (= fx sx) (= (aref *field* (getOffset (- sx 1) (- sy 1))) -1) (= (aref *field* (getOffset (- fx 1) (- fy 1))) -1))
	(progn
	  (setf (slot-value f 'x) (+ fx 1))
	  (setf (slot-value s 'x) (+ fx 1)))))
  (setf *state* 'unpause))

(defun rotateStones (f s)
  (setf *state* 'pause)
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
      ( (< fy sy)(= fx sx)
	(progn
	  (format t "~%2nd")
					;	 (setf (slot-value f 'x) (+ sy 1))
	  (setf (slot-value s 'y) (- sy 1))
	  (setf (slot-value s 'x) (- sx 1))))
      ((> fx sx) (= fy sy)
       (progn
	 (format t "~%3rd")
	 (setf (slot-value s 'x) (+ sx 1))
	 (setf (slot-value s 'y) (- sy 1))))
      ((> fy sy)(= fx sx)
       (progn
	 (format t "~%4th")
	 (setf (slot-value s 'y) (+ sy 1))
	 (setf (slot-value f 'x) (- sx 1))))))
  (format t "~%end rotate stone")
  (setf *state* 'unpause))

(defun getOffset (x y)
  (+ (* y *fieldW*) x))


(defun dropPuyo (p)
  (if (and (< (+ (slot-value p 'y) 1) *fieldH*)
	   (= (aref *field* (getOffset (slot-value p 'x) (+ (slot-value p 'y) 1))) -1))
      (progn
	(setf (slot-value p 'y) (+ (slot-value p 'y) 1)))
      (progn
	(let ((x (slot-value p 'x))
	      (y (slot-value p 'y))
	      (col (slot-value p 'col)))
					;	   (format t "~%setting x=~D y=~D col=~D" x y col)
	  (setf (aref *field* (getOffset x y)) col))
	(setf *state* 'newPuyos))))

(defun updatePosition ()
  "threaded function which will update pos each second"
  (format t "~%updatePosition:here we go")
  (loop do 
     ;;	(format t "~%updatePosition loop")
       (if (eq *state* 'unpause)
	   (progn
	     (dropPuyo *first*) 
	     (dropPuyo *second*)
	     (if (eq *state* 'newPuyos)
		 (progn
		   (copyField *field* *solveField*)
		   (backtrack *solveField* (slot-value *first* 'x) (slot-value *first* 'y) (slot-value *first* 'col))
;;		   (setArray *solveField* -1)
		   (format t "~%matchStones=~D" *matchStones*)
		   (setf *matchStones* 0)
		   

		   ;;		    (backtrack *field* (slot-value *second* 'x) (slot-value *first* 'y) (slot-value *first* 'col))
		   

		   (format t "~%make new puyos")
		   (if (and (/= (aref *field* (getOffset 2 0)) -1) (/= (aref *field* (getOffset 3 0)) -1))
		       (progn		       
			 (setf *run* 0)
			 (lispbuilder-sdl:draw-surface-at-* *gameover* 0 0 :surface lispbuilder-sdl:*default-display*)
			 (lispbuilder-sdl:update-display))
		       (progn
			 (setf (slot-value *first* 'x) 2 )
			 (setf (slot-value *first* 'y) 0 )
			 ;;			 (setf (slot-value *first* 'col) (random *maxCols*))
			 (setf (slot-value *first* 'col) 0)
			 (setf (slot-value *second* 'x) 3 )
			 (setf (slot-value *second* 'y) 0 )
			 ;;			 (setf (slot-value *second* 'col) (random *maxCols*))
			 (setf (slot-value *second* 'col) 0)
			 (setf *state* 'unpause)))))))
     ;;	(format t "~%should drop")
     ;;(sleep 1)
     ;; (let ((ticks (lispbuilder-sdl:sdl-get-ticks)))
     ;; 	 (while (<= (- *lastTicks* ticks) 200000)
     ;; 	   (sleep .1))
     ;; 	 (setf *lastTicks* (lispbuilder-sdl:sdl-get-ticks)))

       (sleep .1)
     while(= 1 *run*))
  (format t "~%updatePosition:end"))

(defun backtrack (f x y col) 
;;  (sleep .5)
  (format t "~%backtrack:: x=~D y=~D col=~D" x y col)
;;  (break)  

;;  (format t "~%col kommt ~D mal vor"   (countColArray f col))
  (if (and (>= x 0) (>= y 0) (< x *fieldW*) (< y *fieldH*))
      (cond
	((=(aref f (getOffset x y)) col)
	 (progn
	   (format t "~%bt setzt marker auf x=~D y=~D" x y)
	   (setf (aref f (getOffset x y)) 255)
	   (setf *matchStones* (+ *matchStones* 1))
	   (setq *coordsList* (append *coordsList* '((x y)))))
	 (if (or
	      (backtrack f (+ x 1) y col)
	      (backtrack f x (+ y 1) col)
	      (backtrack f (- x 1) y col)
	      (backtrack f x (- y 1) col)
	      nil)
	     nil)
	 nil)
	((/=(aref f (getOffset x y)) 255)
	 nil)	
	(nil))))

  
;;game-loop

;;we need to clear the gaming field
(clearField)
;;(setf *lastTicks* (lispbuilder-sdl:sdl-get-ticks))


(format t "~%starting thread")
(bordeaux-threads:make-thread #'updatePosition :name "upos")
(format t "~%starting thread done")

(lispbuilder-sdl:with-events (:poll)
  (:quit-event () T)
  (:key-down-event (:key key)
		   (when (lispbuilder-sdl:key= key :sdl-key-escape)
		     (lispbuilder-sdl:push-quit-event))
		   (when (lispbuilder-sdl:key= key :sdl-key-p)
		     (setf *state* 'pause))
		   (when (lispbuilder-sdl:key= key :sdl-key-u)
		     (setf *state* 'unpause))
		   (when (lispbuilder-sdl:key= key :sdl-key-up)
		     (rotateStones *first* *second*))
		   (when (lispbuilder-sdl:key= key :sdl-key-left)
		     (movetoLeft *first* *second*))
		   (when (lispbuilder-sdl:key= key :sdl-key-right)
		     (moveToRight *first* *second*)))

  (:idle ()
	 ;;	 (format t "~%we are in idle mode")
	 (if (eq *run* 1)
	     (progn
	       (lispbuilder-sdl:clear-display lispbuilder-sdl:*white*)
	       (drawField *field*)
	       (drawPuyo *first*)
	       (drawPuyo *second*)
	       (lispbuilder-sdl:update-display)))))
;;we are done. bye bye
(setf *run* 0)
(sleep 1)
(format t "~%we are done...")
(lispbuilder-sdl:quit-sdl)