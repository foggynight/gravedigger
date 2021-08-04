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

(defparameter *default-max-room-length* 16
  "Default maximum height and width of a room.")

(defparameter *default-squareness-threshold* 0.25
  "Default squareness threshold used in GET-SPLIT-DIRECTION.")

(defun get-random-room-dimensions
    (region
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

(defun get-centered-room-region (region room-dimensions)
  "Get the region representing a room of size ROOM-DIMENSIONS placed in the
center of REGION, this function assumes there is a valid position for the room
to be placed within REGION."
  (let ((offset-y (floor (- (region-height region)
                            (car room-dimensions))
                         2))
        (offset-x (floor (- (region-width region)
                            (cdr room-dimensions))
                         2)))
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
  (let ((tly (region-top-left-y room))
        (tlx (region-top-left-x room))
        (bry (region-bottom-right-y room))
        (brx (region-bottom-right-x room)))
    ;; Add corner tiles
    (setf (tile-symbol (aref (dungeon-tiles dungeon) tly tlx))
          (tile_type->symbol 'cwall)
          (tile-symbol (aref (dungeon-tiles dungeon) tly brx))
          (tile_type->symbol 'cwall)
          (tile-symbol (aref (dungeon-tiles dungeon) bry tlx))
          (tile_type->symbol 'cwall)
          (tile-symbol (aref (dungeon-tiles dungeon) bry brx))
          (tile_type->symbol 'cwall))
    ;; Add left and right wall tiles
    (loop for y from (1+ tly) to (1- bry)
          do (setf (tile-symbol (aref (dungeon-tiles dungeon) y tlx))
                   (tile_type->symbol 'vwall)
                   (tile-symbol (aref (dungeon-tiles dungeon) y brx))
                   (tile_type->symbol 'vwall)))
    ;; Add top and bottom wall tiles
    (loop for x from (1+ tlx) to (1- brx)
          do (setf (tile-symbol (aref (dungeon-tiles dungeon) tly x))
                   (tile_type->symbol 'hwall)
                   (tile-symbol (aref (dungeon-tiles dungeon) bry x))
                   (tile_type->symbol 'hwall)))
    ;; Add floor tiles
    (loop for y from (1+ tly) to (1- bry)
          do (loop for x from (1+ tlx) to (1- brx)
                   do (setf (tile-symbol (aref (dungeon-tiles dungeon) y x))
                            (tile_type->symbol 'floor))))))

(defun add-random-room (dungeon region &optional (random-placement nil))
  "Add a room to the given region of a dungeon. The room is of random size and
is placed in the center of the region unless RANDOM-PLACEMENT is non-nil, in
which case the room is placed at a random position within the region.

This function must be called only once on a given region, as it does not check
for other rooms, only for the boundaries of the region.

If no room can fit within the region, this function does nothing."
  (let* ((room-dimensions (get-random-room-dimensions region))
         (height (car room-dimensions))
         (width (cdr room-dimensions)))
    (unless (or (null height) (null width))
      (add-room-tiles dungeon
                      (if random-placement
                          (get-random-room-region region room-dimensions)
                          (get-centered-room-region region room-dimensions))))))

(defun get-random-deviation (deviation)
  "Get a random value within the range: +/- DEVATION"
  (float (* deviation (/ (- (random 201) 100) 100))))

(defun get-random-center-deviation (deviation)
  "Get a random value within the range: 0.5 +/- DEVIATION"
  (+ 0.5 (get-random-deviation deviation)))

(defun get-split-direction
    (region
     &key (squareness-threshold *default-squareness-threshold*))
  "Get a direction along which to split a region based on its dimensions,
returns the direction represented by a number:
- 0 (horizontal): Split the region into top and bottom sub-regions
- 1 (vertical): Split the region into left and right sub-regions"
  (let ((squareness (region-squareness region)))
    (if (< (abs squareness) squareness-threshold)
        (random 2)
        (if (> squareness 0) 0 1))))

;; TODO Implement this
(defun connect-region-pair (region-pair))

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
               (direction (get-split-direction region))
               (split-region-pair (split-region region direction position)))
          (generate-bsp-rooms :dungeon dungeon
                              :region (car split-region-pair)
                              :recursion-depth (1- recursion-depth))
          (generate-bsp-rooms :dungeon dungeon
                              :region (cdr split-region-pair)
                              :recursion-depth (1- recursion-depth))
          (connect-region-pair split-region-pair))))
  dungeon)
