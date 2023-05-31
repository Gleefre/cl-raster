(defsystem "cl-raster"
  :description ""
  :version "0.0.0"
  :author ("Mikhail Fedorov <fedorov.mikhail.s@gmail.com>"
           "Grolter <varedif.a.s@gmail.com>")
  :license "Apache 2.0"
  :depends-on ("3d-vectors" "parse-float" "zpng")
  :pathname "src"
  :components ((:file "packages")
               (:file "scene")
               (:file "parser")
               (:file "core")
               (:file "image")
               (:file "raster"))
  :in-order-to ((test-op (test-op "cl-raster/tests"))))

(defsystem "cl-raster/tests"
  :description "Test system for cl-raster"
  :author ("Mikhail Fedorov <fedorov.mikhail.s@gmail.com>"
           "Grolter <varedif.a.s@gmail.com>")
  :license "Apache 2.0"
  :depends-on ("cl-raster")
  :pathname "tests"
  :components ((:file "packages")))
