;;;; cl-puyopuyo.lisp

(in-package #:cl-puyopuyo)

(defvar *fieldW* (* 32 6)
  "Global field width var")

(defvar *fieldH* (* 32 12)
  "Global field height var")

(defparameter *field* (make-array (* 6 12)))

(defvar *pauseThread* 0)
(setq *pauseThread* 0)


(setf *random-state* (make-random-state t))


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
(lispbuilder-sdl:window *fieldW* *fieldH*)
(lispbuilder-sdl:init-subsystems lispbuilder-sdl:sdl-init-timer)
(lispbuilder-sdl:clear-display lispbuilder-sdl:*white*)


(defparameter *blue* (lispbuilder-sdl:load-image "puyo_blue.png"))
(defparameter *red* (lispbuilder-sdl:load-image "puyo_red.png"))
(defparameter *green* (lispbuilder-sdl:load-image "puyo_green.png"))
(defparameter *yellow* (lispbuilder-sdl:load-image "puyo_yellow.png"))

(defun drawPuyo (x y col)
  "draws a puyo at coordinates x,y. x,y are puyo field coordinates in the dimension of 6 width and 12 height puyos. due to the fact that we start counting from zero 5,11 is the max coordiante."
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
  (loop for x from 0 to 11 do
       (loop for y from 0 to 5 do
	  ;;	  (format t "~% x=~D y=~D index=~D" x y (+ (* y 12) x))
	    (drawPuyo x y (aref *field* (+ (* y 12) x))))))



(defun moveToLeft ()
  "this updates position of stone one to the left, if possible"
  (format t "~% left"))

(defun dropPuyos ()
  "this function lets the stones drop"
  (format t "~%dropPuyos:here we go")
  (setf *onePosY* (+ *onePosY* 1))
  (setf *twoPosY* (+ *twoPosY* 1))
  )


(defun updatePosition ()
  "threaded function which will update pos each second"
  (format t "~%updatePosition:here we go")
  (loop do 
       (format t "~%updatePosition loop")
       (dropPuyos)
       (sleep 1)
     while(/= 1 *pauseThread*))
    (format t "~%updatePosition:end"))

    
;;this will kick of out thread
(defvar *thr* (bordeaux-threads:make-thread #'updatePosition :name "upos"))

;;game-loop
(lispbuilder-sdl:with-events (:poll)
  (:quit-event () T)
  (:key-down-event (:key key)
		   (when (lispbuilder-sdl:key= key :sdl-key-escape)
		     (lispbuilder-sdl:push-quit-event))
		   (when (lispbuilder-sdl:key= key :sdl-key-left)
		     (moveToLeft))
		   (when (lispbuilder-sdl:key= key :sdl-key-right)
		     (dropPuyos)))
  (:idle ()
	 ;;let's test a bit
	 (clearField)
	 (lispbuilder-sdl:clear-display lispbuilder-sdl:*white*)
	 (drawPuyo *onePosX* *onePosY* *oneCol*)
	 (drawPuyo *twoPosX* *twoPosY* *twoCol*)
	 (drawField)
	 (lispbuilder-sdl:update-display)))

;;we are done. bye bye
;(bordeaux-threads:destroy-thread *thr*)

(setf *pauseThread* 1)
(sleep 1)
(lispbuilder-sdl:quit-sdl)
