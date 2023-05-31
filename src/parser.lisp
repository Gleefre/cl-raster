;;;; Parser of scene files (.obj).

(in-package #:cl-raster/parser)

(defgeneric parse-token (token type)
  (:method (token type) (coerce token type))
  (:method ((token string) (type (eql 'symbol)))
    (find-symbol (string-upcase token) '#:cl-raster/parser))
  (:method ((token string) (type (eql 'keyword)))
    (intern (string-upcase token) '#:keyword))
  (:method ((token string) (type (eql 'id)))
    (parse-integer token))
  (:method ((token string) (type (eql 'float)))
    (parse-float:parse-float token)))

(defparameter *ops* (make-hash-table :test #'equal))

(defun undefined-op (&rest args)
  (declare (ignore args))
  (error "This operation was not defined"))

(defmacro define-op (name (&rest args) &body body)
  (let (lambda-args bindings)
    (dolist (arg args)
      (cond ((eq arg '&optional)
             (push arg lambda-args))
            ((listp (car arg))
             (destructuring-bind ((name type) default-value) arg
               (push `(,name ,default-value) lambda-args)
               (push `(,name (parse-token ,name ',type)) bindings)))
            (T
             (destructuring-bind (name type) arg
               (push name lambda-args)
               (push `(,name (parse-token ,name ',type)) bindings)))))
    `(setf (gethash ',name *ops*)
           (lambda (,@(reverse lambda-args))
             (let (,@bindings)
               ,@body)))))

(defun call-op (op args)
  (apply (gethash (parse-token op 'symbol) *ops* #'undefined-op)
         args))

;;; vertex data

(defvar *v-array*)
(define-op v ((x float) (y float) (z float) &optional ((w float) 1))
  (vector-push-extend (vector x y z w)
                      *v-array*))

(defvar *vt-array*)
(define-op vt ((u float) &optional ((v float) 0) ((w float) 0))
  (vector-push-extend (vector u v w)
                      *vt-array*))

(defvar *vn-array*)
(define-op vn ((x float) (y float) (z float))
  (vector-push-extend (vector x y z)
                      *vn-array*))

(defvar *vp-array*)
(define-op vp ((u float) &optional ((v float) 0) ((w float) 1))
  (vector-push-extend (vector u v w)
                      *vp-array*))

;;; Initialize special variables

(defmacro with-obj-chunk (&body body)
  `(let ((*v-array*  (make-array 0 :adjustable T :fill-pointer 0))
         (*vt-array* (make-array 0 :adjustable T :fill-pointer 0))
         (*vn-array* (make-array 0 :adjustable T :fill-pointer 0))
         (*vp-array* (make-array 0 :adjustable T :fill-pointer 0)))
     ,@body))

;;; Toplevel parse functions

(defun parse-line (line)
  (let* ((line (subseq line 0 (position #\# line)))
         (tokens (remove "" (uiop:split-string line) :test #'string=)))
    (when tokens
      (call-op (car tokens) (cdr tokens)))))

(defun parse-obj (file)
  (with-obj-chunk
    (mapcar #'parse-line (uiop:read-file-lines file))
    (list *v-array*
          *vt-array*
          *vn-array*
          *vp-array*)))
