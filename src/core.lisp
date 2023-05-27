;;;; Rasterization renderer.

(in-package #:cl-raster/core)

(defun project-point-to-camera (camera point)
  (let* ((to-point-vector (v:v- (scene:camera-center camera) point))
         (center-to-canvas-vector (v:v/ to-point-vector
                                        (v:v. to-point-vector
                                              (scene:camera-direction camera))))
         (in-canvas-vector (v:v- center-to-canvas-vector (scene:camera-direction camera)))
         (x-cord-2d (v:v. (scene:camera-x-vector camera) in-canvas-vector))
         (y-cord-2d (v:v. (scene:camera-y-vector camera) in-canvas-vector)))
    (list (v:vec2 (round (* (/ (1+ x-cord-2d) 2) (scene:camera-width camera)))
                  (round (* (/ (1+ y-cord-2d) 2) (scene:camera-height camera))))
          (v:v. to-point-vector
                (v:vunit (scene:camera-direction camera))))))

(defun triangle-contains (triangle point)
  (let ((a->p (v:v- point (first triangle)))
        (b->p (v:v- point (second triangle)))
        (c->p (v:v- point (third triangle)))
        (bc-cross (v:vrot2 (v:v- (second triangle) (third triangle)) (/ pi 2)))
        (ca-cross (v:vrot2 (v:v- (third triangle) (first triangle)) (/ pi 2)))
        (ab-cross (v:vrot2 (v:v- (first triangle) (second triangle)) (/ pi 2))))
    (and (< 0 (* (v:v. a->p bc-cross)
                 (v:v. b->p ca-cross)))
         (< 0 (* (v:v. b->p ca-cross)
                 (v:v. c->p ab-cross))))))

(defun calculate-depth (point2d point-a point-b point-c)
  "Calculate depth of a point given depths of triangle vertices."
  (let ((distance-a (v:v2norm (v:v- point2d
                                    (first point-a))))
        (distance-b (v:v2norm (v:v- point2d
                                    (first point-b))))
        (distance-c (v:v2norm (v:v- point2d
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
             (minimal-x (max 0 (min (v:vx (first a-point))
                                    (v:vx (first b-point))
                                    (v:vx (first c-point)))))
             (maximal-x (min (scene:camera-width camera)
                             (max (v:vx (first a-point))
                                  (v:vx (first b-point))
                                  (v:vx (first c-point)))))
             (minimal-y (max 0 (min (v:vy (first a-point))
                                    (v:vy (first b-point))
                                    (v:vy (first c-point)))))
             (maximal-y (min (scene:camera-height camera)
                             (max (v:vy (first a-point))
                                  (v:vy (first b-point))
                                  (v:vy (first c-point))))))
        (loop for pixel-x from minimal-x to maximal-x
              do (loop for pixel-y from minimal-y to maximal-y
                       when (triangle-contains (list a-point b-point c-point) (list pixel-x pixel-y))
                       do (let ((point-depth (calculate-depth (v:vec2 pixel-x pixel-y)
                                                              a-point
                                                              b-point
                                                              c-point)))
                            (when (or (eq (aref depths pixel-x pixel-y) :infinity)
                                      (< point-depth (aref depths pixel-x pixel-y)))
                              (setf (aref image pixel-x pixel-y) '(255 255 255))
                              (setf (aref depths pixel-x pixel-y) point-depth)))))))
    image))
