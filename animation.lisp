(in-package :crawl)


(defstruct sheet
  (grid (vec2 0 0))
  (cell-size (vec2 0 0))
  (image nil)
  )

(defun grid-cells (grid size cells)
  (mapcar (lambda (index)
            (multiple-value-bind (y x) (floor index (x grid))
              (mult (vec2 x y) size)))
          cells)
  )

(defun sheet-cells (sheet cells)
  (grid-cells (sheet-grid sheet) (sheet-cell-size sheet) cells)
  )

(defstruct frame
  (origin (vec2 0 0))
  (width 20)
  (height 20)
  (delay (/ 1 8))
  )

(defstruct anim
  (index 0)
  (image nil)
  (frames '())
  (timer nil)
  )

(defun anim-frame (anim &optional (index))
  (let* ((frames (anim-frames anim))
         (num-frames (list-length frames))
         (safe-index (if index
                       (mod index num-frames)
                       (anim-index anim))))
    (nth safe-index frames))
  )

(defun anim-duration (anim)
  (apply #'+ (mapcar #'frame-delay (anim-frames anim)))
  )

(defun frame-index-by-time (anim elapsed)
  (let ((offset (mod elapsed (anim-duration anim)))
        (frames (anim-frames anim)))
    (position-if-not (lambda (frame)
                   (> (decf offset (frame-delay frame)) 0))
                 frames))
  )

(defun current-frame (anim)
  (cond
    ((null (anim-timer anim)) (anim-frame anim))
    (t (anim-frame anim (frame-index-by-time anim (elapsed-seconds)))))
  )

(defun draw-anim (anim pos)
  (let ((frame (current-frame anim)))
    (draw-image pos
                (anim-image anim)
                :origin (frame-origin frame)
                :width (frame-width frame)
                :height (frame-height frame)))
  )

(defun anim-cells (sheet &rest cells)
  (with-slots (cell-size) sheet
    (make-anim :image (sheet-image sheet)
               :frames (mapcar (lambda (origin)
                                 (make-frame :origin origin
                                             :width (x cell-size)
                                             :height (y cell-size)))
                               (sheet-cells sheet cells))))
  )
