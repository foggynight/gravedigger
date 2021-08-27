;;;; Dungeon Generation using BSP Rooms
;;
;; Functions used to generate dungeons containing rooms connected by corridors
;; using the BSP (Binary Space Partitioning) Rooms algorithm.
;;
;; Reference: http://www.roguebasin.com/index.php?title=Basic_BSP_Dungeon_generation

(in-package #:gravedigger)

(defparameter *default-min-room-length* 5
  "Default minimum height and width of a room.

Three is the minimum sensible value as rooms must contain at least one walkable
tile, and the walkable tiles must be surrounded by wall and door tiles.")
(defparameter *default-max-room-length* 25
  "Default maximum height and width of a room.")

(defparameter *default-center-deviation* 0.25
  "Default center deviation used to decide what position on which to split a
region in GENERATE-BSP-ROOMS.")
(defparameter *default-recursion-depth* 4
  "Default recursion depth of GENERATE-BSP-ROOMS, determines how many levels of
sub-regions to split the region covering a dungeon into.")
(defparameter *default-squareness-threshold* 0.25
  "Default squareness threshold used to decide what direction to split a region
in GET-SPLIT-DIRECTION and GENERATE-BSP-ROOMS.")

(defun get-random-room-dimensions
    (region
     &key
       (min-room-length *default-min-room-length*)
       (max-room-length *default-max-room-length*))
  "Get the dimensions of a random room whose size is determined by the
dimensions of REGION and the room length parameters.

If no room can fit within REGION, this function returns NIL, otherwise, it
returns a cons pair containing the height and width of a random room.

For a room to fit within REGION, there must be a position within REGION where
the room can be placed such that it lies completely within REGION, with a
one-tile gap between the walls of the room and the edges of REGION."
  (let ((region-height (region-height region))
        (region-width (region-width region)))
    (if (or (< (- region-height 2) min-room-length)
            (< (- region-width 2) min-room-length))
        nil
        (cons (min (+ min-room-length
                      (random (1+ (- (- region-height 2) min-room-length))))
                   max-room-length)
              (min (+ min-room-length
                      (random (1+ (- (- region-width 2) min-room-length))))
                   max-room-length)))))

(defun get-room-region (region room-dimensions &optional (random-placement nil))
  "Get the region representing a room of size ROOM-DIMENSIONS placed in the
center of REGION, this function assumes there is a valid position for the room
to be placed within REGION.

Should RANDOM-PLACEMENT be non-nil, the room region is randomly placed within
REGION such that there is a one-tile gap between the walls of the room and the
edges of REGION, instead of at REGION's center."
  (let ((offset-y nil)
        (offset-x nil))
    (if random-placement
        (setq offset-y (1+ (random (1+ (- (- (region-height region) 2)
                                          (car room-dimensions)))))
              offset-x (1+ (random (1+ (- (- (region-width region) 2)
                                          (cdr room-dimensions))))))
        (setq offset-y (floor (- (region-height region)
                                 (car room-dimensions))
                              2)
              offset-x (floor (- (region-width region)
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
  "Add the floor and wall tiles of ROOM to DUNGEON."
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
  "Add a random room to REGION. The room is of random size and is placed in the
center of the region unless RANDOM-PLACEMENT is non-nil, in which case the room
is placed at a random position within the region.

This function must be called only once on a given region, as it does not check
for other rooms, only for the boundaries of the region.

If no room can fit within REGION, this function does nothing."
  (let ((room-dimensions (get-random-room-dimensions region)))
    (unless (null room-dimensions)
      (add-room-tiles dungeon (get-room-region region
                                               room-dimensions
                                               random-placement)))))

(defun get-random-deviation (deviation)
  "Get a random value within the range: +/- DEVATION"
  (float (* deviation (/ (- (random 201) 100) 100))))

(defun get-random-center-deviation (deviation)
  "Get a random value within the range: 0.5 +/- DEVIATION"
  (+ 0.5 (get-random-deviation deviation)))

(defun get-split-direction
    (region
     &optional (squareness-threshold *default-squareness-threshold*))
  "Get a direction along which to split REGION based on its squareness.

If the squareness of REGION is below SQUARENESS-THRESHOLD, the direction is
chosen randomly, otherwise the chosen direction is that which splits the larger
dimension of REGION.

The direction is represented by a number:
- 0 (horizontal): Split REGION into top and bottom sub-regions
- 1 (vertical): Split REGION into left and right sub-regions"
  (let ((squareness (region-squareness region)))
    (if (< (abs squareness) squareness-threshold)
        (random 2)
        (if (> squareness 0) 0 1))))

;; TODO Implement this
(defun connect-region-pair (region-pair))

(defun generate-bsp-rooms-aux (dungeon
                               region
                               recursion-depth
                               center-deviation
                               squareness-threshold)
  "Auxiliary function to GENERATE-BSP-ROOMS, should not be called on its own!"
  ;; Must check if the region is null and do nothing if so, as region splitting
  ;; in recursively higher level calls to GENERATE-BSP-ROOMS-AUX may result in
  ;; invalid regions should CENTER-DEVIATION be great enough.
  (unless (null region)
    (if (zerop recursion-depth)
        (add-random-room dungeon region)
        (let* ((position (get-random-center-deviation center-deviation))
               (direction (get-split-direction region squareness-threshold))
               (region-pair (split-region region direction position)))
          (generate-bsp-rooms-aux dungeon
                                  (car region-pair)
                                  (1- recursion-depth)
                                  center-deviation
                                  squareness-threshold)
          (generate-bsp-rooms-aux dungeon
                                  (cdr region-pair)
                                  (1- recursion-depth)
                                  center-deviation
                                  squareness-threshold)
          (connect-region-pair region-pair)))))

(defmethod generate-dungeon
    ((type (eql :bsp-rooms))
     &key
       (dungeon (generate-dungeon 'default :symbol #\space))
       (region (dungeon-region dungeon))
       (recursion-depth *default-recursion-depth*)
       (center-deviation *default-center-deviation*)
       (squareness-threshold *default-squareness-threshold*))
  "Generate a dungeon containing rooms connected by corridors using the BSP
Rooms algorithm.

Should a dungeon be passed to the DUNGEON key parameter, that dungeon will be
modified instead of generating a new one.

Should a region be passed to the REGION key parameter, only that region of the
dungeon will be modified."
  (generate-bsp-rooms-aux dungeon
                          region
                          recursion-depth
                          center-deviation
                          squareness-threshold)
  dungeon)
