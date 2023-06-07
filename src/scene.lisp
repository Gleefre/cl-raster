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

(defstruct mtl
  Ka
  Kd
  Ks
  Ns
  Tr
  Tf
  Ni
  illum)

(defstruct triangle
  face
  mtl)

(defstruct scene
  triangles
  light-sources)
