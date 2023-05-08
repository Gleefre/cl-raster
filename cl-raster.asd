(defsystem "cl-raster"
  :description "Mikhail Fedorov <fedorov.mikhail.s@gmail.com>"
  :version "0.0.0"
  :author (""
           "Grolter <varedif.a.s@gmail.com>")
  :license "Apache 2.0"
  :pathname "src"
  :components ((:file "packages"))
  :in-order-to ((test-op (test-op "cl-raster/tests"))))

(defsystem "cl-raster/tests"
  :description "Test system for cl-raster"
  :author ("Mikhail Fedorov <fedorov.mikhail.s@gmail.com>"
           "Grolter <varedif.a.s@gmail.com>")
  :license "Apache 2.0"
  :depends-on ("cl-raster")
  :pathname "tests"
  :components ((:file "packages")))
