(defpackage #:cl-raster/scene
  (:use #:cl)
  (:export #:camera-width)
  (:export #:camera-height)
  (:export #:camera-center)
  (:export #:camera-direction)
  (:export #:scene-triangles))

(defpackage #:cl-raster/parser
  (:use #:cl)
  (:export #:load-scene)
  (:local-nicknames (#:scene #:cl-raster/scene)))

(defpackage #:cl-raster/core
  (:use #:cl)
  (:export #:render)
  (:local-nicknames (#:scene #:cl-raster/scene))
  (:local-nicknames (#:vectors #:3d-vectors)))

(defpackage #:cl-raster/image
  (:use #:cl)
  (:export #:save-png))

(defpackage #:cl-raster
  (:use #:cl)
  (:export #:render)
  (:local-nicknames (#:scene  #:cl-raster/scene)
                    (#:core   #:cl-raster/core)
                    (#:parser #:cl-raster/parser)
                    (#:image  #:cl-raster/image)))
