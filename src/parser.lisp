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

;;; Toplevel parse functions

(defun parse-line (line)
  (let* ((line (subseq line 0 (position #\# line)))
         (tokens (remove "" (uiop:split-string line) :test #'string=)))
    (when tokens
      (call-op (car tokens) (cdr tokens)))))
