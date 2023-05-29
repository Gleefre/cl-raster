;;;; Parser of scene files (.obj).

(in-package #:cl-raster/parser)

(defun raw-line-to-tokens (line)
  (let ((line-no-comments (str:trim
                            (str:substring
                              0
                              (position #\# line)
                              line))))
    (str:split " " line-no-comments :omit-nulls T)))

(defun parse-mtl-file (filename)
  (let ((materials (make-hash-table))
        (current-mtl NIL))
    (with-open-file (in-stream filename)
      (loop for line = (read-line in-stream NIL)
            while line
            for tokens = (raw-line-to-tokens line)
            when tokens
            do (progn
                 (when (eq (first tokens) "newmtl")
                   (setf (gethash (second tokens) (make-mtl))))
                 (when (eq (first tokens) "Ks")
                   (setf (material-Ks (gethash current-mtl))
                         (list (parse-float (second tokens))
                               (parse-float (third tokens))
                               (parse-float (fourth tokens))))))))
    materials))

;;; See https://en.wikipedia.org/wiki/Wavefront_.obj_file

(defun parsei-obj-file (filename)
  (let ((scene (list ()))
        (triangles (list))
        (points (list))
        (points-norm (list))
        (points-texture (list))
        (materials (make-hash-table)))
    (with-open-file (in-stream filename)
      (loop for line = (read-line in-stream NIL)
            while line
            for tokens = (raw-line-to-tokens line)
            when tokens
            do (progn
                 (when (eq (first tokens) "mtllib")
                   (parse-mtl-file (second tokens)))
                 (when (eq (first tokens) "usemtl")
                   (setf current-material (gethash (second tokens) materials)))
                 (when (eq (first tokens) "v")
                   (push (apply #'vectors:vec (cdr tokens)) points))
                 (when (eq (first tokens) "vn")
                   (push (apply #'vectors:vec (cdr tokens)) points-norm))
                 (when (eq (first tokens) "vt")
                   (push (apply #'vectors:vec (cdr tokens)) points-texture))
                 (when (eq (first tokens) "f")
                   (loop for index from 1 to (- (length tokens) 2)
                         do (push (list (nth (parse-integer (nth index tokens)) points)
                                        (nth (parse-integer (nth (+ index 1) tokens)) points)
                                        (nth (parse-integer (nth (+ index 2) tokens)) points))
                                  triangles))))))
    scene))

