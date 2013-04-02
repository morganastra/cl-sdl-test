
(ql:quickload :lispbuilder-sdl)
(ql:quickload :l-math)

(defpackage :surface-gravity
  (:use :common-lisp)
  (:export :main))

(in-package :surface-gravity)

;;; SDL setup
(defparameter *x-res* 1280)
(defparameter *y-res* 720)

(defparameter *pos-color* (sdl:color :r 255 :b 255 :g 255))

(defparameter *scale* 100.0 "Display scale in pixels per meter")
(defparameter *origin* (lm:vector 0 *y-res*) "Pixel position of the coordinate origin")

(defparameter *gravity* (lm:vector 0 -9.8) "Gravitational acceleration")
(defparameter *dampening* 0.8 "Fraction of speed retained in bounces")

(defparameter *all-objects* '() "List of objects tracked by the engine")

;(defclass ball ()
;  ((position :initarg :position :accessor ball-position)
;   (velocity :initarg :velocity :accessor ball-velocity)))

(defstruct ball
  position
  velocity)

(defun integrate (a acceleration dt)
  (let ((velocity (ball-velocity a))
        (position (ball-position a)))
    (make-ball :velocity (lm:+ velocity (lm:* acceleration dt))
               :position (lm:+ position (lm:* velocity dt)))))

;(defun integrate (a acceleration dt)
;  (make-ball :velocity (lm:vector 0 0)
;             :position (lm:- (lm:vector 0 10) (ball-position a))))

(defun bounce (a)
  (let ((velocity (ball-velocity a))
        (position (ball-position a)))
    (make-ball :velocity (lm:* *dampening* (lm:* velocity (lm:vector 0 -1)))
               :position position)))

(defun pixels->meters (pixel-coords)
  (lm:* (lm:vector 1 -1)
        (lm:/ (lm:- pixel-coords *origin*) *scale*)))

(defun meters->pixels (meter-coords)
  (let ((coords (lm:+ (lm:* (lm:vector 1 -1) (lm:* meter-coords *scale*)) *origin*)))
    (sdl:point :x (round (lm:x coords))
               :y (round (lm:y coords)))))

(defun update-world (dt)
  (setf *all-objects* (bounce-objects (integrate-objects *all-objects* dt))))

(defun integrate-objects (objects dt) 
  (mapcar (lambda (obj) (integrate obj *gravity* dt)) objects))

(defun bounce-objects (objects)
  (mapcar (lambda (obj)
            (if (<= (lm:elt (ball-position obj) 1) 0)
                (bounce obj)
                obj))
            objects))

(defun on-screen-p (obj)
  (let ((x (elt obj 0))
        (y (elt obj 1)))
    (and (>= x 0) (<= x *x-res*)
         (>= y 0) (<= y *y-res*))))

(defun draw-world ()
  (dolist (a *all-objects*)
    (let ((p (meters->pixels (ball-position a))))
      (if (on-screen-p p)
          (sdl:draw-filled-circle p 10 :color sdl:*white*)))))

(defun surface-gravity-game-window (x-res y-res)
  (sdl:with-init ()
    (sdl:window x-res y-res
                :fps (make-instance 'sdl:fps-timestep)
                :title-caption "Click anywhere to create balls")
    (setf (sdl:frame-rate) 60)
    (sdl:with-events ()
      (:quit-event () t)
      (:mouse-button-down-event (:x x :y y)
                                (push (make-ball :position (pixels->meters (list x y))
                                                 :velocity (lm:vector 0 0))
                                      *all-objects*))
      (:idle ()
             (sdl:clear-display sdl:*black*)
             (sdl:with-timestep ()
                                (update-world (/ (sdl:dt) 1000)))
             (draw-world)
             (sdl:update-display)))))             

(defun main ()
  (surface-gravity-game-window *x-res* *y-res*))









