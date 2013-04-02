(ql:quickload 'lispbuilder-sdl)

(defparameter *x-res* 1280)
(defparameter *y-res* 720)

(defparameter *random-color* (sdl:color :r 255 :g 255 :b 255))

(defun game-window (x-res y-res)
  (sdl:with-init ()
    (sdl:window x-res y-res :title-caption "This is a fun game, have fun damnit!")
    (setf (sdl:frame-rate) 60)

    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event ()
                       (sdl:push-quit-event))
      (:idle ()
             ;; Change the color of the box if the left mouse button is depressed
             (when (sdl:mouse-left-p)
               (setf *random-color* (sdl:color :r (round (* 255 (/ (sdl:mouse-x) x-res)))
                                               :g (round (* 255 (/ (sdl:mouse-y) y-res)))
                                               :b 127)))
             ;; Clear the display each loop
             ;(sdl:clear-display sdl:*black*)
             ;; Draw the box having a center at the mouse x/y coords
             (sdl:draw-box (sdl:rectangle-from-midpoint-* (sdl:mouse-x) (sdl:mouse-y) 20 20)
                           :color *random-color*)
             ;; Redraw the display
             (sdl:Update-display)))))

