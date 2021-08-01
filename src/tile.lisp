(in-package :gravedigger)

(defparameter *default-tile-symbol* #\space)

(defparameter *tile-type-symbol-alist*
  '((floor  . #\.)
    (cwall  . #\+)
    (hwall  . #\-)
    (vwall  . #\|)
    (stairs . #\%)
    (void   . #\space))
  "Alist of tile symbol and type pairs.")

(defstruct tile
  "Tile structure representing a tile in a dungeon."
  (symbol *default-tile-symbol* :type standard-char))

(defun make-tile-list2 (height width &optional (symbol *default-tile-symbol*))
  "Make a new list2 of tiles."
  (loop for y from 0 below height
        collect (loop for x from 0 below width
                      collect (make-tile :symbol symbol))))

(defun make-tile-array2 (height width &optional (symbol *default-tile-symbol*))
  "Make a new array2 of tiles."
  (make-array (list height width)
              :initial-contents (make-tile-list2 height width symbol)))

(defun get-tile-symbol (tile-type)
  (cdr (assoc tile-type *tile-type-symbol-alist*)))
