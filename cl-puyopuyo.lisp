(in-package #:cl-puyopuyo)

(format t "~%init...")


;;puyos are represented in an simple 6*12 array. with this it is easy to backtrack later neighboring puyos
(defvar *fieldW* 6)
(defvar *fieldH* 12)
(defparameter *field* (make-array (* *fieldW* *fieldH*)))

(setf *random-state* (make-random-state t))

(defvar *run* 1)
(setq *run* 1)

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
  "draws a puyo at coordinates x,y. x,y are puyo field coordinates in the dimension of 6 width and 12 height puyos. due to the fact that we start counting from zero 5,11 is the max coordiante."

  (let ((x (slot-value puyo 'x))
	(y (slot-value puyo 'y))
	(col (slot-value puyo 'col)))
    (format t "~%drawPuyo:: x=~D and y=~D and col=~D" x y col)
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

(defun moveToLeft (puyo)
  (format t "moveToLeft"))

(defun moveToRight (puyo)
  (format t "moveToRight"))


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
       (dropPuyo *first*)
       (dropPuyo *second*)
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
		   (when (lispbuilder-sdl:key= key :sdl-key-left)
		     (movetoLeft *first* )
		     (movetoLeft *second* ))
		   (when (lispbuilder-sdl:key= key :sdl-key-right)
		     (moveToRight *first*)
		     (moveToRight *second*)))		   
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
