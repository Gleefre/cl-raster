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
  "Calculates depth of the point when it is inside of the triangle.
   Returns NIL otherwise."
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
              for point-x from (+ 1/2 minimal-x)
              do (loop for pixel-y from minimal-y to maximal-y
                       for point-y from (+ 1/2 minimal-y)
                       for point-depth-p = (triangle-point-depth-p flat-triangle (v:vec2 point-x point-y))
                       when point-depth-p
                       do (when (or (eq (aref depths pixel-x pixel-y) :infinity)
                                    (< point-depth-p (aref depths pixel-x pixel-y)))
                            (setf (aref image pixel-x pixel-y) '(255 255 255)
                                  (aref depths pixel-x pixel-y) point-depth-p))))))
    image))
