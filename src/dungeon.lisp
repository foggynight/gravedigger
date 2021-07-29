(in-package :gravedigger)

(defparameter *default-dungeon-height* 22)
(defparameter *default-dungeon-width* 80)

(defstruct dungeon
  "Dungeon structure representing a dungeon in the world."
  tiles)

(defun generate-dungeon (&optional
                           (height *default-dungeon-height*)
                           (width *default-dungeon-width*)
                           (symbol *default-tile-symbol*))
  "Generate a new dungeon."
  (make-dungeon :tiles (make-tile-array2 height width
                                         :symbol *default-tile-symbol*)))

(defun dungeon-height (dungeon)
  "Get the height of a dungeon in number of tiles."
  (array-dimension (dungeon-tiles dungeon) 0))

(defun dungeon-width (dungeon)
  "Get the width of a dungeon in number of tiles."
  (array-dimension (dungeon-tiles dungeon) 1))

(defun print-dungeon (dungeon)
  "Print the text representation of a dungeon to standard output."
  (let ((tiles (dungeon-tiles dungeon)))
    (fresh-line)
    (dotimes (y (array-dimension tiles 0))
      (dotimes (x (array-dimension tiles 1))
        (princ (tile-symbol (aref tiles y x))))
      (terpri))))
