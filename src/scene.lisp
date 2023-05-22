;;;; Scene objects definitions.

(in-package #:cl-raster/scene)

(defstruct camera
  width
  height
  center
  direction
  x-vector
  y-vector)

(defstruct light-source
  color
  point)

(defstruct scene
  triangles
  light-sources)
