(in-package #:cl-puyopuyo)

;;puyos are represented in an simple 6*12 array. with this it is easy to backtrack later neighboring puyos
(defvar *fieldW* 6)
(defvar *fieldH* 12)
(defparameter *field* (make-array (* *fieldW* *fieldH*)))

(setf *random-state* (make-random-state t))

(defvar *run* 1)


;we should make clos-objects
(defvar *oneCol* 0 )
(defvar *onePosX* 0)
(defvar *onePosY* 0)
(setf *oneCol* (random 4))
(setf *onePosX* 2)
(setf *onePosY* 0)

(defvar *twoCol* 0 )
(defvar *twoPosX* 0)
(defvar *twoPosY* 0)
(setf *twoCol* (random 4))
(setf *twoPosX* 3)
(setf *twoPosY* 0)


(lispbuilder-sdl:init-video)
;;the puyo sprites are 32x32 pixels

(lispbuilder-sdl:window  (* 32 *fieldW*) (* 32 *fieldH*) )
(lispbuilder-sdl:init-subsystems lispbuilder-sdl:sdl-init-timer)
(lispbuilder-sdl:clear-display lispbuilder-sdl:*white*)


(defparameter *blue* (lispbuilder-sdl:load-image "puyo_blue.png"))
(defparameter *red* (lispbuilder-sdl:load-image "puyo_red.png"))
(defparameter *green* (lispbuilder-sdl:load-image "puyo_green.png"))
(defparameter *yellow* (lispbuilder-sdl:load-image "puyo_yellow.png"))

(defun drawPuyo (x y col)
  "draws a puyo at coordinates x,y. x,y are puyo field coordinates in the dimension of 6 width and 12 height puyos. due to the fact that we start counting from zero 5,11 is the max coordiante."
  (format t "~%drawPuyo:: x=~D and y=~D and col=~D" x y col)
  (if (and (< x *fieldW*) (< y *fieldH*) (< col 4))
      (case col
	(0 (lispbuilder-sdl:draw-surface-at-* *blue* (* x 32) (* y 32)  :surface lispbuilder-sdl:*default-display*))
	(1 (lispbuilder-sdl:draw-surface-at-* *red* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*))
	(2 (lispbuilder-sdl:draw-surface-at-* *green* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*))
	(3 (lispbuilder-sdl:draw-surface-at-* *yellow* (* x 32) (* y 32) :surface lispbuilder-sdl:*default-display*)))
      (print "out of bounds")))

(defun clearField ()
  (loop for x from 0 to 11 do
       (loop for y from 0 to 5 do
	  ;;	  (format t "~% x=~D y=~D index=~D" x y (+ (* y 12) y))
	    (setf (aref *field* (+ (* y 12) x)) -1))))

(defun drawField ()
  (loop for x from 0 to 5 do
       (loop for y from 0 to 11 do
	    (drawPuyo x y (aref *field* (getOffset x y))))))



(defun moveToLeft ()
  "this updates position of stone one to the left, if possible"
  (format t "~% left")
  (if (and (< *onePosX* *fieldW*) (>= *onePosX* 0) (< *twoPosX* *fieldW*) (>= *twoPosX* 0))
      (progn
	(setf *onePosX* (- *onePosX* 1))
	(setf *twoPosX* (- *twoPosX* 1)))))

(defun moveToRight ()
  "this updates position of stone one to the right, if possible"
  (format t "~% right")
  (if (and (< *onePosX* *fieldW*) (>= *onePosX* 0) (< *twoPosX* *fieldW*) (>= *twoPosX* 0))
      (progn
	(setf *onePosX* (+ *onePosX* 1))
	(setf *twoPosX* (+ *twoPosX* 1)))))

(defun getOffset (x y)
  (format t "~%getOffset:: x=~D y=~D offset=~D" x y (+ (* 6 y) x))
  (+ (* y 6) x))


;;(format t "~%dropPuyos: onePosY=~D twoPosY=~D arefOne=~D" *onePosY* *twoPosY* (aref *field* (getOffset *onePosX* (+ *onePosY* 1))))


(defun dropPuyos ()
  "this function lets the stones drop"
  (if (and (< (+ *onePosY* 1) *fieldH*) (< (+ *twoPosY* 1) *fieldH*)
	   (= (aref *field* (getOffset *onePosX* (+ *onePosY* 1))) -1)
	   (= (aref *field* (getOffset *twoPosX* (+ *twoPosY* 1))) -1))
      (progn
	(format t "~%drop is true")
	(setf *twoPosY* (+ *twoPosY* 1))
	(setf *onePosY* (+ *onePosY* 1)))))
  



(defun updatePosition ()
  "function which will update pos each second"
  (format t "~%updatePosition:here we go")
  (loop do 
       (dropPuyos)
       (sleep 1)
     while (/= 1 *run*)))

;;game-loop
(clearField)
(lispbuilder-sdl:with-events (:poll)
  (:quit-event () T)
  (:key-down-event (:key key)
		   (when (lispbuilder-sdl:key= key :sdl-key-escape)
		     (lispbuilder-sdl:push-quit-event))
		   (when (lispbuilder-sdl:key= key :sdl-key-left)
		     (moveToLeft))
		   (when (lispbuilder-sdl:key= key :sdl-key-right)
		     (moveToRight)))
  (:idle ()
	 ;;let's test a bit
	 (format t "~%we are in idle mode")
	 (updatePosition)
	 (lispbuilder-sdl:clear-display lispbuilder-sdl:*white*)
	 (drawPuyo *onePosX* *onePosY* *oneCol*)
	 (drawPuyo *twoPosX* *twoPosY* *twoCol*)
	 (drawField)
	 (lispbuilder-sdl:update-display)))

;;we are done. bye bye
(setf *run* 0)
(format t "~%we are done...")
(lispbuilder-sdl:quit-sdl)
