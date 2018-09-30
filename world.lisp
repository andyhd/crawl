(in-package :crawl)


(defvar *world* nil)

(defmethod render-layer ((layer tile-layer))
  (dolist (cell (layer-cells layer))
    (let* ((tile (cell-tile cell))
           (tileset (tile-tileset tile))
           (tileset-rows (floor (tileset-tile-count tileset)
                                (tileset-columns tileset)))
           (tileset-height (* 32 tileset-rows))
           (origin (vec2 (coerce (+ 0 (tile-pixel-x tile)) 'integer)
                         (coerce (- tileset-height (+ 32 (tile-pixel-y tile))) 'integer))))
      (draw-image (vec2 (coerce (cell-x cell) 'integer)
                        (coerce (- (map-height-pixels *world*) (cell-y cell)) 'integer))
                  :tiles
                  :origin origin
                  :width 32
                  :height 32))
    )
  )

(defmethod render-layer ((layer image-layer))
  nil)

(defmethod render-layer ((layer object-layer))
  (dolist (object (object-group-objects layer))
    (declare (ignore object))))

(defmethod render-layer ((layer group-layer))
  (dolist (layer (group-layers layer))
    (render-layer layer)))

(defun render-world ()
  (dolist (layer (map-layers *world*))
    (render-layer layer)))

