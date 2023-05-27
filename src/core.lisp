;;;; Rasterization renderer.

(in-package #:cl-raster/core)

(defstruct depth-point
  point
  depth)

(defun d-p-x (point)
  (v:vx (depth-point-point point)))

(defun d-p-y (point)
  (v:vy (depth-point-point point)))

(defun project-point-to-camera (camera point)
  (let* ((O->p (v:v- (scene:camera-center camera) point))
         (O->p* (v:v/ O->p
                      (v:v. O->p
                            (scene:camera-direction camera))))
         (o*->p* (v:v- O->p* (scene:camera-direction camera)))
         (x (v:v. (scene:camera-x-vector camera) o*->p*))
         (y (v:v. (scene:camera-y-vector camera) o*->p*)))
    (make-depth-point :point
                      (v:vec2 (round (* (/ (1+ x) 2) (scene:camera-width camera)))
                              (round (* (/ (1+ y) 2) (scene:camera-height camera))))
                      :depth
                      (v:v. O->p
                            (v:vunit (scene:camera-direction camera))))))

(defun trg-to-camera (camera trg)
  (mapcar (lambda (point)
            (project-point-to-camera camera point))
          trg))

(defun triangle-contains (triangle point)
  (let ((a->p (v:v- point (depth-point-point (first triangle))))
        (b->p (v:v- point (depth-point-point (second triangle))))
        (c->p (v:v- point (depth-point-point (third triangle))))
        (bc-cross (v:vrot2 (v:v- (depth-point-point (second triangle))
                                 (depth-point-point (third triangle)))
                           (/ pi 2)))
        (ca-cross (v:vrot2 (v:v- (depth-point-point (third triangle))
                                 (depth-point-point (first triangle)))
                           (/ pi 2)))
        (ab-cross (v:vrot2 (v:v- (depth-point-point (first triangle))
                                 (depth-point-point (second triangle)))
                           (/ pi 2))))
    (and (< 0 (* (v:v. a->p bc-cross)
                 (v:v. b->p ca-cross)))
         (< 0 (* (v:v. b->p ca-cross)
                 (v:v. c->p ab-cross))))))

(defun calculate-depth (point2d triangle)
  "Calculate depth of a point given depths of triangle vertices."
  (let ((distance-a (v:v2norm (v:v- point2d
                                    (depth-point-point (first triangle)))))
        (distance-b (v:v2norm (v:v- point2d
                                    (depth-point-point (second triangle)))))
        (distance-c (v:v2norm (v:v- point2d
                                    (depth-point-point (third triangle))))))
    (/ (+ (* (depth-point-depth (first triangle)) distance-b distance-c)
          (* (depth-point-depth (second triangle)) distance-c distance-a)
          (* (depth-point-depth (third triangle)) distance-a distance-b))
       (+ (* distance-a distance-b)
          (* distance-b distance-c)
          (* distance-c distance-a)))))

(defun render (scene camera)
  (let ((image (make-array (list (scene:camera-width camera) (scene:camera-height camera))
                           :initial-element '(0 0 0)))
        (depths (make-array (list (scene:camera-width camera) (scene:camera-height camera))
                            :initial-element :infinity)))
    (dolist (triangle (scene:scene-triangles scene))
      (let* ((flat-triangle (trg-to-camera camera triangle))
             (minimal-x (max 0 (reduce #'min triangle :key #'d-p-x)))
             (maximal-x (min (scene:camera-width camera)
                             (reduce #'max triangle :key #'d-p-x)))
             (minimal-y (max 0 (reduce #'min triangle :key #'d-p-y)))
             (maximal-y (min (scene:camera-height camera)
                             (reduce #'max triangle :key #'d-p-y))))
        (loop for pixel-x from minimal-x to maximal-x
              do (loop for pixel-y from minimal-y to maximal-y
                       when (triangle-contains flat-triangle (v:vec2 pixel-x pixel-y))
                       do (let ((point-depth (calculate-depth (v:vec2 pixel-x pixel-y)
                                                              flat-triangle)))
                            (when (or (eq (aref depths pixel-x pixel-y) :infinity)
                                      (< point-depth (aref depths pixel-x pixel-y)))
                              (setf (aref image pixel-x pixel-y) '(255 255 255))
                              (setf (aref depths pixel-x pixel-y) point-depth)))))))
    image))
