;;;; Goban - A board for playing Go

(ql:quickload :lispbuilder-sdl)

;; (defpackage :goban
;;   (:use :common-lisp)
;;   (:export :run-goban))

;;; Game engine parameters
(defparameter *board-size* 19)

(defparameter *board* (make-array (list *board-size* *board-size*) :initial-element nil)
  "Current board state.")

(defparameter *moves* '()
  "List of moves from beginning of the game.")

;;; Display parameters
(defparameter *scale* 37 "Number of pixels per goban cell")

(defparameter *x-res* (* *board-size* *scale*))
(defparameter *y-res* (* *board-size* *scale*))


;;; 
(defun apply-move-to-board (move board)
  (destructuring-bind (piece h v) move 
      (setf (aref board h v) piece)))

(defun move-if-possible (h v board moves)
  (if (not (aref board h v))
      (progn
        (push (list (who-goes-this-turn) h v)
              moves)
        (apply-move-to-board (car moves) board))))

(let ((current 'black))
  (defun who-goes-this-turn (&optional who)
    (setf current (or who
                      (case current
                        (black 'white)
                        (white 'black))))))


;;; Transformations between game engine and SDL graphics layer.
;;;
;;; Graphics layer coordinates are called x and y. Goban coordinates are called
;;; h and v (for horizontal and vertical).
(defun x->h (x &optional scale (offset 0))
  (let ((scale (or scale *scale*))
        (offset-x (- x offset)))
    (floor offset-x scale)))

(defun h->x (h &optional scale (offset 0))
  (let ((scale (or scale *scale*)))
    (+ offset
       (round (+ (* h scale)     ;; This would go to the edge of the cell, so add
                 (/ scale 2)))))) ;; half a cell to get to the middle.

(defun piece->color (piece)
  (case piece
    (black sdl:*black*)
    (white sdl:*white*)))

;;; SDL Input handling functions
(defun handle-button-press (button x y)
  (let ((h (x->h x *scale*))
        (v (x->h y *scale*)))
    (move-if-possible h v *board* *moves*)))

;;; SDL drawing functions
(defun draw-piece (piece h v scale)
  (sdl:draw-filled-circle (sdl:point :x (h->x h scale)
                                     :y (h->x v scale))
                          (floor scale 2)
                          :color (piece->color piece)))

(defun draw-lines ()
  (dotimes (i *board-size*)
    ;; Vertical lines:
    (sdl:draw-line (sdl:point :x (h->x 0)                 :y (h->x i))
                   (sdl:point :x (h->x (1- *board-size*)) :y (h->x i))
                   :color sdl:*black*)
    ;; Horizontal lines:
    (sdl:draw-line (sdl:point :x (h->x i) :y (h->x 0))
                   (sdl:point :x (h->x i) :y (h->x (1- *board-size*)))
                   :color sdl:*black*)))

(defun draw-board (board)
  (draw-lines)
  (dotimes (h *board-size*)
    (dotimes (v *board-size*)
      (let ((piece (aref board h v)))
        (if piece
            (draw-piece piece h v *scale*))))))






(defun go-game-window (x-res y-res)
  "Create SDL window and run the game."
  (sdl:with-init ()
    (sdl:window x-res y-res
                :title-caption "Lisp Goban!")
    (setf (sdl:frame-rate) 60)
    (sdl:with-events ()
      (:mouse-button-down-event (:button button :x x :y y)
                                (handle-button-press button x y))
      (:quit-event () t)
      (:idle ()
             (sdl:clear-display (sdl:color :R 165 :G 82 :B 82))
             (draw-board *board*)
             (sdl:update-display)))))


(defun run-goban ()
  (go-game-window *x-res* *y-res*))
