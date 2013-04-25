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

(let ((current 'black))
  (defun who-goes-this-turn (&optional who)
    (setf current (or who
                      (case current
                        (black 'white)
                        (white 'black))))))

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Experimental group-based code


;;; A piece is a length-three list of the form (color h v), where color is
;;; either 'black or 'white and h and v are numbers.
;;; A group is a list of adjacent pieces. They must all have the same color.
;;; The *groups* parameter is a list of groups.
(defparameter *groups* '())

(defun unnest (list)
  "Remove one level of nesting from a nested list."
  (loop for elt in list appending elt))

(defun get-piece-at-location (location groups)
  "Return the piece at location, or nil if there is not one."
  (find location (unnest groups) :test #'equalp :key #'cdr))

(defun location-in-group (location group)
  "Test whether a group contains a piece at the given location"
  (find location group :test #'equalp :key #'cdr))

(defun get-group-at-location (location groups)
  "Return the group containing a piece at the given location, or nil if there
is not a piece there."
  (find-if #'(lambda (group)
               (location-in-group location group))
           groups))

(defun move-if-possible (piece groups)
  ""
  (destructuring-bind (color . loc) piece
    (if (not (get-piece-at-location loc groups))
        (push (combine-groups
               color
               (mapcar #'(lambda (loc*)
                           (get-group-at-location loc* groups))
                       (find-neighbors loc)))
         groups))))

(defun combine-groups (color list-of-groups)
  "Combine groups of a given color."
  (reduce #'union
          (remove-if #'(lambda (group-color)
                         (not (equalp color group-color)))
                     list-of-groups
                     :key #'caar)))






(defun get-group-adjacencies (group)
  "Return the set of locations adjacent to a given group."
  )



(defun count-group-liberties (group groups)
  "Return the number of liberties the given group has.")

(defun pretty-print-board (groups)
  ""
  ) 
  

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

