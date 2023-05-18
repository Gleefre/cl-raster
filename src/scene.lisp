;;;; Scene objects definitions.

(in-package #:cl-raster/scene)

(defstruct camera
  width
  height
  center
  direction)

(defstruct light-source
  color
  point)

(defstruct scene
  triangles
  light-sources)
