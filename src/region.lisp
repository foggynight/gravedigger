(in-package :gravedigger)

(defstruct region
  "Region structure representing a region of a dungeon, used to divide a dungeon
into regions and sub-regions.

Top-left and bottom-right refer to the corresponding corner tiles of the region,
and are represented by a cons containing a y-x pair of coordinates."
  (top-left '(0 . 0) :type cons)
  (bottom-right '(0 . 0) :type cons))

(defun region-top-left-y (region)
  "Get the y coordinate of the top-left corner tile of a region."
  (car (region-top-left region)))

(defun region-top-left-x (region)
  "Get the x coordinate of the top-left corner tile of a region."
  (cdr (region-top-left region)))

(defun region-bottom-right-y (region)
  "Get the y coordinate of the bottom-right corner tile of a region."
  (car (region-bottom-right region)))

(defun region-bottom-right-x (region)
  "Get the x coordinate of the bottom-right corner tile of a region."
  (cdr (region-bottom-right region)))

(defun region-height (region)
  "Get the height of a region."
  (1+ (- (region-bottom-right-y region) (region-top-left-y region))))

(defun region-width (region)
  "Get the width of a region."
  (1+ (- (region-bottom-right-x region) (region-top-left-x region))))

(defun verify-region (region)
  "Verify that a region is valid, i.e. are the coordinates of the top-left
corner tile less-than or equal-to the coordinates of the bottom-right corner
tile.

Returns nil if the region is invalid, otherwise returns the region."
  (if (or (null region)
          (> (region-top-left-y region) (region-bottom-right-y region))
          (> (region-top-left-x region) (region-bottom-right-x region)))
      nil
      region))

(defun verify-sub-region (region sub-region)
  "Verify that a sub-region is a valid sub-region of another region, i.e. does
the sub-region lie completely within the other region.

This function expects that the other region has already been verified to be a
valid region.

Returns nil if the sub-region is invalid, otherwise returns the sub-region."
  (if (or (null sub-region)
          (< (region-top-left-y sub-region) (region-top-left-y region))
          (< (region-top-left-x sub-region) (region-top-left-x region))
          (> (region-bottom-right-y sub-region) (region-bottom-right-y region))
          (> (region-bottom-right-x sub-region) (region-bottom-right-x region))
          (> (region-top-left-y sub-region) (region-bottom-right-y region))
          (> (region-top-left-x sub-region) (region-bottom-right-x region))
          (< (region-bottom-right-y sub-region) (region-top-left-y region))
          (< (region-bottom-right-x sub-region) (region-top-left-x region)))
      nil
      sub-region))

(defun split-region-horizontally (region position)
  "Horizontally split a region into top and bottom sub-regions and return them
as a cons pair.

This is an auxiliary function of SPLIT-REGION and should not be called outside
of it, as this function does not check the validity of its arguments."
  (let ((height (1+ (- (region-bottom-right-y region)
                       (region-top-left-y region)))))
    (cons (verify-sub-region region
                             (make-region :top-left (region-top-left region)
                                          :bottom-right (cons (1- (+ (region-top-left-y region)
                                                                     (floor (* height position))))
                                                              (region-bottom-right-x region))))
          (verify-sub-region region
                             (make-region :top-left (cons (+ (region-top-left-y region)
                                                             (floor (* height position)))
                                                          (region-top-left-x region))
                                          :bottom-right (region-bottom-right region))))))

(defun split-region-vertically (region position)
  "Vertically split a region into left and right sub-regions and return them as
a cons pair.

This is an auxiliary function of SPLIT-REGION and should not be called outside
of it, as this function does not check the validity of its arguments."
  (let ((width (1+ (- (region-bottom-right-x region)
                      (region-top-left-x region)))))
    (cons (verify-sub-region region
                             (make-region :top-left (region-top-left region)
                                          :bottom-right (cons (region-bottom-right-y region)
                                                              (1- (+ (region-top-left-x region)
                                                                     (floor (* width position)))))))
          (verify-sub-region region
                             (make-region :top-left (cons (region-top-left-y region)
                                                          (+ (region-top-left-x region)
                                                             (floor (* width position))))
                                          :bottom-right (region-bottom-right region))))))

(defun split-region (region direction position)
  "Split a region into two sub-regions and return them as a cons pair.

DIRECTION determines the direction with which to split the region:
- 0 (horizontal): Split the region into top and bottom sub-regions
- 1 (vertical): Split the region into left and right sub-regions

POSITION refers to the position on which to split the region, x position for
vertical, y position for horizontal, and is represented by a decimal number
between 0 and 1, corresponding to the relative position within the region.

Should the split result in one of the sub-regions being invalid, nil will be
returned for that sub-region. This may cause the cons pair returned from this
function to appear to be a list if it is the second sub-region which is invalid.

e.g.

(split-region (make-region :top-left '(0 . 0) :bottom-right '(3 . 3)) 0 0.25)
=> (#S(REGION :TOP-LEFT (0 . 0) :BOTTOM-RIGHT (0 . 3))
    . #S(REGION :TOP-LEFT (1 . 0) :BOTTOM-RIGHT (3 . 3)))

(split-region (make-region :top-left '(0 . 0) :bottom-right '(4 . 4)) 1 0.5)
=> (#S(REGION :TOP-LEFT (0 . 0) :BOTTOM-RIGHT (4 . 1))
    . #S(REGION :TOP-LEFT (0 . 2) :BOTTOM-RIGHT (4 . 4)))

(split-region (make-region :top-left '(0 . 0) :bottom-right '(3 . 3)) 0 0.0)
=> (NIL . #S(REGION :TOP-LEFT (0 . 0) :BOTTOM-RIGHT (3 . 3)))

(split-region (make-region :top-left '(0 . 0) :bottom-right '(3 . 3)) 0 1.0)
=> (#S(REGION :TOP-LEFT (0 . 0) :BOTTOM-RIGHT (3 . 3))) "
  (unless (verify-region region)
    (error (format nil "Error: Invalid region: ~A" region)))
  (when (or (< position 0)
            (> position 1))
    (error (format nil "Error: Invalid position: ~A" position)))
  (cond ((zerop direction) (split-region-horizontally region position))
        ((= direction 1) (split-region-vertically region position))
        (t (error (format nil "Error: Invalid direction: ~A" direction)))))
