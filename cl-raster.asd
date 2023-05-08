(defsystem "cl-raster"
  :description ""
  :version "0.0.0"
  :author (""
           "Grolter <varedif.a.s@gmail.com>")
  :license "Apache 2.0"
  :pathname "src"
  :components ((:file "packages"))
  :in-order-to ((test-op (test-op "cl-raster/tests"))))

(defsystem "cl-raster/tests"
  :description "Test system for cl-raster"
  :author (""
           "Grolter <varedif.a.s@gmail.com>")
  :license "Apache 2.0"
  :depends-on ("cl-raster")
  :pathname "tests"
  :components ((:file "packages")))
