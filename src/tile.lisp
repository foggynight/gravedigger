(in-package :gravedigger)

(defstruct tile
  "Tile structure representing a tile in a dungeon."
  (symbol #\? :type standard-char))

(defun make-tile-list2 (height width)
  "Make a new list2 of tiles."
  (loop for y from 0 below height
        collect (loop for x from 0 below width
                      collect (make-tile))))

(defun make-tile-array2 (height width)
  "Make a new array2 of tiles."
  (make-array (list height width)
              :initial-contents (make-tile-list2 height width)))
