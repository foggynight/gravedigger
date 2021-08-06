(in-package :gravedigger)

(defparameter *default-dungeon-height* 22
  "Default height of a dungeon in number of tiles.")
(defparameter *default-dungeon-width* 80
  "Default width of a dungeon in number of tiles.")

(defstruct dungeon
  "Dungeon structure representing a dungeon in the world."
  (tiles #2A() :type simple-array))

(defun generate-dungeon
    (&key
       (height *default-dungeon-height*)
       (width *default-dungeon-width*)
       (symbol *default-tile-symbol*))
  "Generate a new dungeon."
  (make-dungeon :tiles (make-tile-array2 height width symbol)))

(defun dungeon-height (dungeon)
  "Get the height of a dungeon in number of tiles."
  (array-dimension (dungeon-tiles dungeon) 0))

(defun dungeon-width (dungeon)
  "Get the width of a dungeon in number of tiles."
  (array-dimension (dungeon-tiles dungeon) 1))

(defun set-dungeon-region-tile-symbol (dungeon region &optional (symbol nil))
  "Set the tile symbol of the tiles within the given region of a dungeon, should
the symbol be omitted, a random lowercase letter will be used instead."
  (when (null symbol)
    (flet ((get-random-character () (code-char (+ (random 26) (char-code #\a)))))
      (setq symbol (get-random-character))))
  (loop for y from (region-top-left-y region)
          to (region-bottom-right-y region)
        do (loop for x from (region-top-left-x region)
                   to (region-bottom-right-x region)
                 do (setf (tile-symbol (aref (dungeon-tiles dungeon) y x))
                          symbol))))

(defun print-dungeon (dungeon &optional (stream *standard-output*))
  "Print the text representation of a dungeon to STREAM."
  (let ((tiles (dungeon-tiles dungeon)))
    (fresh-line stream)
    (dotimes (y (array-dimension tiles 0))
      (dotimes (x (array-dimension tiles 1))
        (princ (tile-symbol (aref tiles y x)) stream))
      (terpri stream))))
