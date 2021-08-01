;;;; Dungeon Generation using BSP Rooms
;;
;; Functions used to generate dungeons containing rooms connected by corridors
;; using the BSP (Binary Space Partitioning) Rooms algorithm.
;;
;; Reference: http://www.roguebasin.com/index.php?title=Basic_BSP_Dungeon_generation

(in-package :gravedigger)

(defparameter *default-min-room-length* 5
  "Default minimum height and width of a room.

Three is the minimum sensible value as rooms must contain at least one walkable
tile, and the walkable tiles must be surrounded by wall and door tiles.")

(defparameter *default-max-room-length* 5
  "Default maximum height and width of a room.")

(defun get-random-room-dimensions (region
                                   &key
                                     (min-room-length *default-min-room-length*)
                                     (max-room-length *default-max-room-length*))
  "Get a y-x pair of random room dimensions based on the room length parameters
and the dimensions of REGION.

If either dimension of REGION is less than MIN-ROOM-LENGTH, this function
returns NIL, otherwise, it returns a cons pair containing the height and width
of a random room that can fit within REGION."
  (let ((region-height (region-height region))
        (region-width (region-width region)))
    (if (or (< region-height min-room-length)
            (< region-width min-room-length))
        nil
        (cons (min (+ min-room-length
                      (random (1+ (- region-height min-room-length))))
                   max-room-length)
              (min (+ min-room-length
                      (random (1+ (- region-width min-room-length))))
                   max-room-length)))))

;; TODO Add centering factor
(defun get-random-room-region (region room-dimensions)
  "Get the region representing a room of size ROOM-DIMENSIONS placed at a random
position within REGION, this function assumes there is a valid position for the
room to be placed within REGION."
  (let ((offset-y (random (1+ (- (region-height region)
                                 (car room-dimensions)))))
        (offset-x (random (1+ (- (region-width region)
                                 (cdr room-dimensions))))))
    (make-region :top-left (cons (+ (region-top-left-y region) offset-y)
                                 (+ (region-top-left-x region) offset-x))
                 :bottom-right (cons (+ (region-top-left-y region)
                                        offset-y
                                        (1- (car room-dimensions)))
                                     (+ (region-top-left-x region)
                                        offset-x
                                        (1- (cdr room-dimensions)))))))

(defun add-room-tiles (dungeon room)
  "Add the floor and wall tiles of a room to a dungeon."
  ;; Add corner tiles
  (let ((tly (region-top-left-y room))
        (tlx (region-top-left-x room))
        (bry (region-bottom-right-y room))
        (brx (region-bottom-right-x room)))
    (setf (tile-symbol (aref (dungeon-tiles dungeon) tly tlx))
          (get-tile-symbol 'cwall))
    (setf (tile-symbol (aref (dungeon-tiles dungeon) tly brx))
          (get-tile-symbol 'cwall))
    (setf (tile-symbol (aref (dungeon-tiles dungeon) bry tlx))
          (get-tile-symbol 'cwall))
    (setf (tile-symbol (aref (dungeon-tiles dungeon) bry brx))
          (get-tile-symbol 'cwall))
    ;; Add left and right wall tiles
    (loop for y from (1+ tly) to (1- bry)
          do (setf (tile-symbol (aref (dungeon-tiles dungeon) y tlx))
                   (get-tile-symbol 'vwall)))
    (loop for y from (1+ tly) to (1- bry)
          do (setf (tile-symbol (aref (dungeon-tiles dungeon) y brx))
                   (get-tile-symbol 'vwall)))
    ;; Add top and bottom wall tiles
    (loop for x from (1+ tlx) to (1- brx)
          do (setf (tile-symbol (aref (dungeon-tiles dungeon) tly x))
                   (get-tile-symbol 'hwall)))
    (loop for x from (1+ tlx) to (1- brx)
          do (setf (tile-symbol (aref (dungeon-tiles dungeon) bry x))
                   (get-tile-symbol 'hwall)))
    ;; Add floor tiles
    (loop for y from (1+ tly) to (1- bry)
          do (loop for x from (1+ tlx) to (1- brx)
                   do (setf (tile-symbol (aref (dungeon-tiles dungeon) y x))
                            (get-tile-symbol 'floor))))))

(defun add-random-room (dungeon region)
  "Add a room to the given region of a dungeon, the room is of random size and
placed at a random position such that it fits within the region, if no room can
fit within the region, this function does nothing.

This function must be called only once on a given region, as it does not check
for other rooms, only for the boundaries of the region."
  (let* ((room-dimensions (get-random-room-dimensions region))
         (height (car room-dimensions))
         (width (cdr room-dimensions)))
    (unless (or (null height) (null width))
      (add-room-tiles dungeon (get-random-room-region region room-dimensions)))))

(defun get-random-deviation (deviation)
  "Get a random value within the range: +/- DEVATION."
  (float (* deviation (/ (- (random 201) 100) 100))))

(defun get-random-center-deviation (center-deviation)
  "Get a random value within the range: 0.5 +/- CENTER-DEVIATION."
  (+ 0.5 (get-random-deviation center-deviation)))

;; TODO Implement this
(defun connect-region-pair (region-pair))

;; TODO Improve the distribution of splits and room placement, currently, the
;; splitting often leads to long narrow rooms that are packed together.
(defun generate-bsp-rooms
    (&key
       (dungeon (generate-dungeon :symbol #\space))
       (region (make-region :top-left (cons 0 0)
                            :bottom-right (cons (1- (dungeon-height dungeon))
                                                (1- (dungeon-width dungeon)))))
       (center-deviation 0.25)
       (recursion-depth 4))
  "Generate a dungeon containing rooms connected by corridors using the BSP
Rooms algorithm.

Optionally, a dungeon may be passed as a key argument to this function, this
function will then modify that dungeon instead of generating a new one."
  (unless (null region)
    (if (zerop recursion-depth)
        (add-random-room dungeon region)
        (let* ((position (get-random-center-deviation center-deviation))
               (direction (random 2))
               (split-region-pair (split-region region direction position)))
          (generate-bsp-rooms :dungeon dungeon
                              :region (car split-region-pair)
                              :recursion-depth (1- recursion-depth))
          (generate-bsp-rooms :dungeon dungeon
                              :region (cdr split-region-pair)
                              :recursion-depth (1- recursion-depth))
          (connect-region-pair split-region-pair))))
  dungeon)
