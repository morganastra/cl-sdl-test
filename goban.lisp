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
(defun get-piece-color (location board)
  "Get the piece symbol at a given location on the board."
  (destructuring-bind (h v) location
    (aref board h v)))

(defun apply-move-to-board (move board)
  (destructuring-bind (piece (h v)) move 
      (setf (aref board h v) piece)))

(defun move-if-possible (location board moves)
  "Place a piece at location on board (updating the moves list as appropriate),
unless there is already a piece occupying that location."
  (if (not (get-piece-color location board))
      (progn
        (push (list (who-goes-this-turn) location)
              moves)
        (apply-move-to-board (car moves) board))))

(let ((current 'black))
  (defun who-goes-this-turn (&optional who)
    (setf current (or who
                      (case current
                        (black 'white)
                        (white 'black))))))

(defun find-neighbors (location)
  "Given the coordinates of an intersection, return the coordinates of the
neighboring intersections."
  (destructuring-bind (h v) location
    (remove-if (lambda (i) (or (member -1 i)
                               (member 19 i)))
               (list (list (1+ h) v)
                     (list (1- h) v)
                     (list h (1+ v))
                     (list h (1- v))))))


(defun find-whole-group-containing (location board)
  "Given the coordinates of a stone, return a list of the coordinates of
all stones in its group"
  ;; Dear future me, the reference to color in the lambda down there is
  ;; why we had to use let* instead of let
  (let* ((color (get-piece-color location board))
         (group '(location))
         (stack (remove-if-not  
                 (lambda (l) 
                   (equal color (get-piece-color l board)))
                 (find-neighbors location))))
    (print color) ;DEBUG
    (print group) ;DEBUG
    (print stack) ;DEBUG
    (mapcar (lambda ()) stack))) ;; Recurse onto each piece on the stack



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transformations between game engine and SDL graphics layer.
;;;
;;; Graphics layer coordinates are called x and y. Goban coordinates are
;;; called h and v (for horizontal and vertical), but should be passed around
;;; as a pair canonically referred to as location.
(defun xy->location (x y &optional (scale *scale*) (x-offset 0) (y-offset 0))
  "Transform SDL x and y coordinates into an (h v) location pair."
  (defun x->h (x offset)
    (let ((relative-x (- x offset)))
      (floor relative-x scale)))
  (list (x->h x x-offset)
        (x->h y y-offset)))

(defun location->xy (location &optional (scale *scale*)
                                  (x-offset 0) (y-offset 0))
  "Transform a location (h v) pair into SDL x and y coordinates."
  (defun h->x (h offset)
    (+ offset
       (round (+ (* h scale)      ;; This would go to the edge of the cell,
                 (/ scale 2)))))  ;; so add half a cell to get to the middle.
  (destructuring-bind (h v) location
    (list (h->x h x-offset)
          (h->x v y-offset)))) 

(defun piece-color->sdl-color (piece-symbol)
  "Get the SDL color value associated with a game piece color."
  (case piece-symbol
    (black sdl:*black*)
    (white sdl:*white*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SDL stuff!
;;; SDL Input handling functions
(defun handle-button-press (button x y)
  (if (equalp button sdl:sdl-button-left)
      (move-if-possible (xy->location x y) *board* *moves*)))

;;; SDL drawing functions
(defun draw-piece (location scale)
  "Draw the piece at location on the board."
  (destructuring-bind (x y) (location->xy location scale)
    (sdl:draw-filled-circle (sdl:point :x x 
                                       :y y)
                            (floor scale 2)
                            :color (piece-color->sdl-color
                                    (get-piece-color location *board*)))))

(defun draw-lines ()
  (destructuring-bind ((x-min y-min) (x-max y-max))
      (list (location->xy (list 0 0))
            (location->xy (list *board-size* *board-size*)))
    (dotimes (i *board-size*)
      (destructuring-bind (x y) (location-xy (list i i))
        ;; Vertical lines:
        (sdl:draw-line (sdl:point :x x-min :y x)
                       (sdl:point :x x-max :y x)
                       :color sdl:*black*)
        ;; Horizontal lines:
        (sdl:draw-line (sdl:point :x y :y y-min)
                       (sdl:point :x y :y y-max)
                       :color sdl:*black*)))))

(defun draw-board (board)
  "Draw everything."
  (draw-lines)
  (dotimes (h *board-size*)
    (dotimes (v *board-size*)
      (let ((piece (get-piece-color (list h v) board)))
        (if piece
            (draw-piece piece location *scale*))))))





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
