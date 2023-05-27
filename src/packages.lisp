(defpackage #:cl-raster/scene
  (:use #:cl)
  (:export #:camera
           #:camera-width
           #:camera-height
           #:camera-center
           #:camera-direction
           #:camera-x-vector
           #:camera-y-vector
           #:make-camera
           #:camera-p
           #:scene
           #:scene-triangles
           #:scene-light-sources
           #:make-scene
           #:scene-p
           #:light-source
           #:light-source-color
           #:light-source-point
           #:make-light-source
           #:light-source-p))

(defpackage #:cl-raster/parser
  (:use #:cl)
  (:export #:load-scene)
  (:local-nicknames (#:scene #:cl-raster/scene)
                    (#:vectors #:3d-vectors)))  

(defpackage #:cl-raster/core
  (:use #:cl)
  (:export #:render)
  (:local-nicknames (#:scene #:cl-raster/scene)
                    (#:v #:3d-vectors)))

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
