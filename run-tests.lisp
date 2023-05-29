#!/usr/bin/env -S sbcl --script

(require 'uiop)
(require 'asdf)

(let ((init-file (merge-pathnames ".sbclrc" (user-homedir-pathname))))
  (when (probe-file init-file)
    (load init-file)))

(asdf:load-asd (merge-pathnames "cl-raster.asd" (uiop:getcwd)))

(ql:quickload :alexandria)

(mapc #'ql-dist:ensure-installed
      (alexandria:flatten
       (mapcar #'ql-dist:dependency-tree
               (apply #'append
                      (mapcar #'asdf:system-depends-on
                              (mapcar #'asdf:find-system
                                      '(:cl-raster :cl-raster/tests)))))))

(let ((asdf:*compile-file-warnings-behaviour* :error))
  (asdf:load-system :cl-raster :force T)
  (asdf:load-system :cl-raster/tests :force T)
  (asdf:test-system :cl-raster/tests))

(quit)
