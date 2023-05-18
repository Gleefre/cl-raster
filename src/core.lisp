;;;; Rasterization renderer.

(in-package #:cl-raster/core)

(defun project-point-to-camera (camera point)
  (let* ((to-point-vector (vectors:v- (scene:camera-center camera) point))
         (center-to-canvas-vector (vectors:v/ to-point-vector (vectors:v. to-point-vector (scene:camera-direction camera))))
         (in-canvas-vector (vectors:v- center-to-canvas-vector (scene:camera-direction camera)))
         (x-canvas-coordinate (vectors:v. (scene:camera-x-vector camera) in-canvas-vector))
         (y-canvas-coordinate (vectors:v. (scene:camera-y-vector camera) in-canvas-vector)))
    (vectors:vec2 (round (* (/ (1+ x-canvas-coordinate) 2) (scene:camera-width camera)))
		 (round (* (/ (1+ y-canvas-coordinate) 2) (scene:camera-height camera))))))

(defun triangle-contains (triangle point)
  (let ((a-to-point (vectors:v- point (first triangle)))
        (b-to-point (vectors:v- point (second triangle)))
        (c-to-point (vectors:v- point (third triangle)))
        (bc-perpendicular (vectors:vrot2 (vectors:v- (second triangle) (third triangle)) (/ pi 2)))
        (ca-perpendicular (vectors:vrot2 (vectors:v- (third triangle) (first triangle)) (/ pi 2)))
        (ab-perpendicular (vectors:vrot2 (vectors:v- (first triangle) (second triangle)) (/ pi 2))))
    (and (< 0 (* (vectors:v. a-to-point bc-perpendicular)
                 (vectors:v. b-to-point ca-perpendicular)))
         (< 0 (* (vectors:v. b-to-point ca-perpendicular)
                 (vectors:v. c-to-point ab-perpendicular))))))

(defun render (scene camera)
    (let ((image (make-array (list (scene:camera-width camera) (scene:camera-height camera))
                             :initial-element '(0 0 0)))
          (depths (make-array (list (scene:camera-width camera) (scene:camera-height camera))
                              :initial-element NIL)))
      (loop for triangle in (scene:scene-triangles scene)
            do (let* ((a-point (project-point-to-camera camera (first triangle)))
                      (b-point (project-point-to-camera camera (second triangle)))
                      (c-point (project-point-to-camera camera (third triangle)))
                      (minimal-x (max 0 (min
					  (first a-point)
					  (first b-point)
					  (first c-point))))
                      (maximal-x (min (scene:camera-width camera)
				      (max
					(first a-point)
					(first b-point)
					(first c-point))))
                      (minimal-y (max 0 (min
					  (second a-point)
					  (second b-point)
					  (second c-point))))
                      (maximal-y (min (camera-height camera)
				      (max
					(second a-point)
					(second b-point)
					(second c-point)))))
                 (loop for pixel-x from minimal-x to maximal-x
                       do (loop for pixel-y from minimal-y to maximal-y
				when (triangle-contains (list a-point b-point c-point) (list pixel-x pixel-y))
				do (setf (aref image pixel-x pixel-y) '(255 255 255))))))
      image))
