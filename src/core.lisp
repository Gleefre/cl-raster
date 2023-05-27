;;;; Rasterization renderer.

(in-package #:cl-raster/core)

(defun project-point-to-camera (camera point)
  (let* ((to-point-vector (vectors:v- (scene:camera-center camera) point))
         (center-to-canvas-vector (vectors:v/ to-point-vector
                                              (vectors:v. to-point-vector
                                                          (scene:camera-direction camera))))
         (in-canvas-vector (vectors:v- center-to-canvas-vector (scene:camera-direction camera)))
         (x-canvas-coordinate (vectors:v. (scene:camera-x-vector camera) in-canvas-vector))
         (y-canvas-coordinate (vectors:v. (scene:camera-y-vector camera) in-canvas-vector)))
    (list (vectors:vec2 (round (* (/ (1+ x-canvas-coordinate) 2) (scene:camera-width camera)))
                        (round (* (/ (1+ y-canvas-coordinate) 2) (scene:camera-height camera))))
          (vectors:v. to-point-vector
                      (vectors:vunit (scene:camera-direction camera))))))

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

(defun calculate-depth (point2d point-a point-b point-c)
  "Calculate depth of a point given depths of triangle vertices."
  (let ((distance-a (vectors:v2norm (vectors:v- point2d
                                                (first point-a))))
        (distance-b (vectors:v2norm (vectors:v- point2d
                                                (first point-b))))
        (distance-c (vectors:v2norm (vectors:v- point2d
                                                (first point-c)))))
    (/ (+ (* (second point-a) distance-b distance-c)
          (* (second point-b) distance-c distance-a)
          (* (second point-c) distance-a distance-b))
       (+ (* distance-a distance-b)
          (* distance-b distance-c)
          (* distance-c distance-a)))))

(defun render (scene camera)
  (let ((image (make-array (list (scene:camera-width camera) (scene:camera-height camera))
                           :initial-element '(0 0 0)))
        (depths (make-array (list (scene:camera-width camera) (scene:camera-height camera))
                            :initial-element :infinity)))
    (dolist (triangle (scene:scene-triangles scene))
      (let* ((a-point (project-point-to-camera camera (first triangle)))
             (b-point (project-point-to-camera camera (second triangle)))
             (c-point (project-point-to-camera camera (third triangle)))
             (minimal-x (max 0 (min (vectors:vx (first a-point))
                                    (vectors:vx (first b-point))
                                    (vectors:vx (first c-point)))))
             (maximal-x (min (scene:camera-width camera)
                             (max (vectors:vx (first a-point))
                                  (vectors:vx (first b-point))
                                  (vectors:vx (first c-point)))))
             (minimal-y (max 0 (min (vectors:vy (first a-point))
                                    (vectors:vy (first b-point))
                                    (vectors:vy (first c-point)))))
             (maximal-y (min (scene:camera-height camera)
                             (max (vectors:vy (first a-point))
                                  (vectors:vy (first b-point))
                                  (vectors:vy (first c-point))))))
        (loop for pixel-x from minimal-x to maximal-x
              do (loop for pixel-y from minimal-y to maximal-y
                       when (triangle-contains (list a-point b-point c-point) (list pixel-x pixel-y))
                       do (let ((point-depth (calculate-depth
                                               (vectors:vec2 pixel-x pixel-y)
                                               a-point
                                               b-point
                                               c-point)))
                            (when (or (eq (aref depths pixel-x pixel-y) :infinity)
                                      (< point-depth (aref depths pixel-x pixel-y)))
                              (setf (aref image pixel-x pixel-y) '(255 255 255))
                              (setf (aref depths pixel-x pixel-y) point-depth)))))))
    image))
