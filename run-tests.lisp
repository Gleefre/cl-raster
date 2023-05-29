#!/usr/bin/env -S sbcl --script

(require 'uiop)
(require 'asdf)

(let ((init-file (merge-pathnames ".sbclrc" (user-homedir-pathname))))
  (when (probe-file init-file)
    (load init-file)))

(mapc (lambda (system)
        (mapc #'ql-dist:ensure-installed
              (remove nil
                      (mapcar #'ql-dist:find-system
                              (asdf:system-depends-on (asdf:find-system system))))))
      '(:cl-raster :cl-raster/tests))

(asdf:load-asd (merge-pathnames "cl-raster.asd" (uiop:getcwd)))

(let ((asdf:*compile-file-warnings-behaviour* :error))
  (asdf:load-system :cl-raster :force T)
  (asdf:load-system :cl-raster/tests :force T)
  (asdf:test-system :cl-raster/tests))

(quit)
