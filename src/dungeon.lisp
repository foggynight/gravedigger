(in-package :gravedigger)

(defparameter *default-dungeon-height* 22
  "Default height of a dungeon in number of tiles.")
(defparameter *default-dungeon-width* 80
  "Default width of a dungeon in number of tiles.")

(defstruct dungeon
  "Dungeon structure representing a dungeon in the world."
  (tiles #2A() :type simple-array))

(defmethod generate-dungeon
    ((type (eql 'default))
     &key
       (height *default-dungeon-height*)
       (width *default-dungeon-width*)
       (symbol *default-tile-symbol*))
  "Generate a default dungeon."
  (make-dungeon :tiles (make-tile-array2 height width symbol)))

(defun dungeon-height (dungeon)
  "Get the height of a dungeon in number of tiles."
  (array-dimension (dungeon-tiles dungeon) 0))

(defun dungeon-width (dungeon)
  "Get the width of a dungeon in number of tiles."
  (array-dimension (dungeon-tiles dungeon) 1))

(defun print-dungeon (dungeon &optional (stream *standard-output*))
  "Print the text representation of a dungeon to STREAM."
  (let ((tiles (dungeon-tiles dungeon)))
    (fresh-line stream)
    (dotimes (y (array-dimension tiles 0))
      (dotimes (x (array-dimension tiles 1))
        (princ (tile-symbol (aref tiles y x)) stream))
      (terpri stream))))

(flet ((fresh-char-set ()
         (loop for i from 0 to 25
               collect (code-char (+ i (char-code #\a))))))
  (let ((char-set (fresh-char-set)))
    (defun set-dungeon-region-tile-symbol (dungeon region &optional symbol)
      "Set the tile symbol of the tiles within the given region of a dungeon.

Should SYMBOL be omitted, a lowercase letter will be used instead. This letter
is selected from the front of a set that initially contains the lowercase
alphabet, the letters are removed upon selection, and the set is refilled when
there are no remaining letters."
      (when (null symbol)
        (when (endp char-set)
          (setq char-set (fresh-char-set)))
        (setq symbol (car char-set))
        (setq char-set (cdr char-set)))
      (loop for y from (region-top-left-y region)
              to (region-bottom-right-y region)
            do (loop for x from (region-top-left-x region)
                       to (region-bottom-right-x region)
                     do (setf (tile-symbol (aref (dungeon-tiles dungeon) y x))
                              symbol))))))
