(in-package #:cl-puyopuyo)

(format t "~%init...")


;;puyos are represented in an simple 6*12 array. with this it is easy to backtrack later neighboring puyos
(defvar *fieldW* 6)
(defvar *fieldH* 12)
(defparameter *field* (make-array (* *fieldW* *fieldH*)))

(setf *random-state* (make-random-state t))

(defvar *state* nil)
(setf *state* 'unpause)

(defvar *run* nil)
(setf *run* 1)


;a puyo consits of col 0-3 (blue, red, green and yellow and a position
(defstruct puyo
  x
  y
  col)

;;we need to stones, 1st and 2nd
;;these need to be global, so that the updatePosition thread can access the stones
(defvar *first* nil)
(defvar *second* nil)

(setf *first*  (make-puyo :x 2 :y 0 :col (random 4)))
(setf *second* (make-puyo :x 3 :y 0 :col (random 4)))


(lispbuilder-sdl:init-video)
;;the puyo sprites are 32x32 pixels

(lispbuilder-sdl:window  (* 32 *fieldW*) (* 32 *fieldH*) )
(lispbuilder-sdl:init-subsystems lispbuilder-sdl:sdl-init-timer)
(lispbuilder-sdl:clear-display lispbuilder-sdl:*white*)


(defparameter *blue* (lispbuilder-sdl:load-image "puyo_blue.png"))
(defparameter *red* (lispbuilder-sdl:load-image "puyo_red.png"))
(defparameter *green* (lispbuilder-sdl:load-image "puyo_green.png"))
(defparameter *yellow* (lispbuilder-sdl:load-image "puyo_yellow.png"))

(defun drawPuyo (puyo)
  (let ((x (slot-value puyo 'x))
	(y (slot-value puyo 'y))
	(col (slot-value puyo 'col)))
;;    (format t "~%drawPuyo:: x=~D and y=~D and col=~D" x y col)
    (if (and (< x *fieldW*) (< y *fieldH*) (< col 4))
	(case col
	  (0 (lispbuilder-sdl:draw-surface-at-* *blue* (* x 32) (* y 32)  :surface lispbuilder-sdl:*default-display*))
	  (1 (lispbuilder-sdl:draw-surface-at-* *red* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*))
	  (2 (lispbuilder-sdl:draw-surface-at-* *green* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*))
	  (3 (lispbuilder-sdl:draw-surface-at-* *yellow* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*))))))

(defun clearField ()
  (loop for y from 0 to 11 do
       (loop for x from 0 to 5 do	    
	    (setf (aref *field* (getOffset x y)) -1))))



(defun moveToLeft (f s)
  (setf *state* 'pause)
  (let ((fx (slot-value f 'x))
	(fy (slot-value f 'y))
	(sy (slot-value s 'y))
	(sx (slot-value s 'x)))
    ;;check if fx<sx, field x-1 is empty and if we are in bounds
    (if (and (< fx sx) (> fx 0) (>= sx 0)(= (aref *field* (getOffset (- fx 1) fy)) -1))
	(progn
	  (setf (slot-value f 'x) (- fx 1))
	  (setf (slot-value s 'x) (- sx 1))))

   (if (and (> fx sx) (>= fx 0) (> sx 0)(= (aref *field* (getOffset (- fx 1) fy)) -1))
	(progn
	  (setf (slot-value f 'x) (- fx 1))
	  (setf (slot-value s 'x) (- sx 1)))))
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
	  (setf (slot-value f 'x) (+ fx 1)))))
  (setf *state* 'unpause))

(defun rotateStones (f s)
  (setf *state* 'pause)
  (let ((fx (slot-value f 'x))
	(fy (slot-value f 'y))
	(sy (slot-value s 'y))
	(sx (slot-value s 'x)))
    (if (< fx sx)
	(progn
	  (setf (slot-value f 'y) (- sy 1))
	  (setf (slot-value f 'x) sx)))	
    (if (>= fx sx)
;	(format t "~% wir sind hier du doof")
	(progn
	  (setf (slot-value s 'x) (- sx 1))
	  (setf (slot-value s 'y) (- sy 1)))))
  ;;(set (slot-value f 'y) (+ fy 1)))))
  (setf *state* 'unpause))
  



(defun getOffset (x y)
 ;; (format t "~%getOffset:: x=~D y=~D offset=~D" x y (+ (* 6 y) x))
  (+ (* y 6) x))


;;(format t "~%dropPuyos: onePosY=~D twoPosY=~D arefOne=~D" *onePosY* *twoPosY* (aref *field* (getOffset *onePosX* (+ *onePosY* 1))))


(defun dropPuyo (puyo)
  (if (and (< (+ (getYpos puyo) 1) *fieldH*)
	   (= (aref *field* (getOffset (getXpos puyo) (+ (getYpos puyo) 1))) -1))
      (setf (slot-value puyo 'y) (+ (slot-value puyo 'y) 1))))
  
(defun updatePosition ()
  "threaded function which will update pos each second"
  (format t "~%updatePosition:here we go")
  (loop do 
       (format t "~%updatePosition loop")
       (if (eq *state* 'unpause)
	   (progn
	     (dropPuyo *first*)
	     (dropPuyo *second*)))
       (sleep 1)
     while(= 1 *run*))
  (format t "~%updatePosition:end"))



(defun getCol (puyo)
  (slot-value puyo 'col)) 

(defun getXpos (puyo)
  (slot-value puyo 'x))

(defun getYpos (puyo)
  (slot-value puyo 'y))

;;game-loop

;;we need to clear the gaming field
(clearField)



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
	 (lispbuilder-sdl:clear-display lispbuilder-sdl:*white*)
	 (drawPuyo *first*)
	 (drawPuyo *second*)
	 (clearField)
	 (lispbuilder-sdl:update-display)))


;;we are done. bye bye
(setf *run* 0)
(sleep 1)
(format t "~%we are done...")
(lispbuilder-sdl:quit-sdl)
