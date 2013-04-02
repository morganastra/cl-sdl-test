(ql:quickload 'lispbuilder-sdl)

(defparameter *x-res* 1280)
(defparameter *y-res* 720)

(defun report-coords-window (x-res y-res)
  (sdl:with-init ()
    (sdl:window x-res y-res :title-caption "Click anywhere")
    (setf (sdl:frame-rate) 60)
    (sdl:with-events ()
      (:quit-event () t)
      (:mouse-button-down-event (:x x :y y)
                                (print (list x y)))
      (:idle ()
             (sdl:update-display)))))

