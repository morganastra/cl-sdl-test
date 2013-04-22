;;;; Goban - A board for playing Go

(ql:quickload :lispbuilder-sdl)

;; (defpackage :goban
;;   (:use :common-lisp)
;;   (:export :run-goban))

;;; Game engine parameters
(defparameter *board-size* 19)

(defparameter *board*
  (make-array (list *board-size* *board-size*) :initial-element nil)
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

(defun find-neighbors (h v)
  "Given the coordinates of an intersection, return the coordinates of the
neighboring intersections."
  (remove-if (lambda (i) (or (member -1 i)
                              (member 19 i)))
             (list (list (1+ h) v)
                   (list (1- h) v)
                   (list h (1+ v))
                   (list h (1- v)))))


(defun find-whole-group-containing (h v board)
  "Given the coordinates of a stone, return a list of the coordinates of
all stones in its group"
  (let* ((color (aref board h v))
         (group (list (list h v)))
         (stack (remove-if-not ;; 
                 (lambda (piece)
                   (destructuring-bind (h v) piece
                     (equal color (aref board h v))))
                 (find-neighbors h v))))
    (print color) ;DEBUG
    (print group) ;DEBUG
    (print stack) ;DEBUG
    (mapcar (lambda ()) stack)))

;; (defun count-group-liberties (h v)
;;   "Count the liberties of the group that includes the piece at h v.")



;;; Transformations between game engine and SDL graphics layer.
;;;
;;; Graphics layer coordinates are called x and y. Goban coordinates are
;;; called h and v (for horizontal and vertical).
(defun xy->board-pos (x y &optional (scale *scale*) (x-offset 0) (y-offset 0))
  (defun x->h (x offset)
    (let ((relative-x (- x offset)))
      (floor relative-x scale)))
  (list (x->h x x-offset)
        (x->h y y-offset)))

(defun board-pos->xy (board-pos &optional (scale *scale*)
                                  (x-offset 0) (y-offset 0))
  (defun h->x (h offset)
    (+ offset
       (round (+ (* h scale)      ;; This would go to the edge of the cell,
                 (/ scale 2)))))  ;; so add half a cell to get to the middle.
  (destructuring-bind (h v) board-pos
    (list (h->x h x-offset)
          (h->x v y-offset)))) 

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
