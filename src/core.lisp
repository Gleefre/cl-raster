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
    (make-depth-point :point (v:vec2 (* (/ (1+ x) 2) (scene:camera-width camera))
                                     (* (/ (1+ y) 2) (scene:camera-height camera)))
                      :depth (v:v. O->p
                                   (v:vunit (scene:camera-direction camera))))))

(defun project-triangle-to-camera (camera triangle)
  (mapcar (lambda (point)
            (project-point-to-camera camera point))
          triangle))

(defun cross-product-2d (a b)
  (- (* (v:vx a) (v:vy b))
     (* (v:vy a) (v:vx b))))

(defun triangle-point-depth-p (triangle point)
  (destructuring-bind (a b c)
      (mapcar #'depth-point-point triangle)
    (let* ((AP (v:v- point A))
           (BP (v:v- point B))
           (CP (v:v- point C))
           (s/a (cross-product-2d BP CP))
           (s/b (cross-product-2d CP AP))
           (s/c (cross-product-2d AP BP))
           (s   (cross-product-2d (v:v- A B)
                                  (v:v- A C))))
      (when (= (float-sign s/a)
               (float-sign s/b)
               (float-sign s/c))
        (/ (+ (* s/a (depth-point-depth (first  triangle)))
              (* s/b (depth-point-depth (second triangle)))
              (* s/c (depth-point-depth (third  triangle))))
           s)))))

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
      (let* ((flat-triangle (project-triangle-to-camera camera triangle))
             (minimal-x (max 0 (floor (reduce #'min flat-triangle :key #'d-p-x))))
             (maximal-x (min (1- (scene:camera-width camera))
                             (ceiling (reduce #'max flat-triangle :key #'d-p-x))))
             (minimal-y (max 0 (floor (reduce #'min flat-triangle :key #'d-p-y))))
             (maximal-y (min (1- (scene:camera-height camera))
                             (ceiling (reduce #'max flat-triangle :key #'d-p-y)))))
        (loop for pixel-x from minimal-x to maximal-x
              do (loop for pixel-y from minimal-y to maximal-y
                       for point-depth = (triangle-point-depth-p flat-triangle (v:vec2 pixel-x pixel-y))
                       when point-depth
                       do (when (or (eq (aref depths pixel-x pixel-y) :infinity)
                                    (< point-depth (aref depths pixel-x pixel-y)))
                            (setf (aref image pixel-x pixel-y) '(255 255 255)
                                  (aref depths pixel-x pixel-y) point-depth))))))
    image))
