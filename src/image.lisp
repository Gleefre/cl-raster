;;;; Saving to image.

(in-package #:cl-raster/image)

(defun save-png (image filename &key if-exists)
  (let* ((width (array-dimension image 0))
         (height (array-dimension image 1))
         (png (make-instance 'zpng:pixel-streamed-png
                             :color-type :truecolor
                             :width width
                             :height height)))
    (with-open-file (stream filename
                            :direction :output
                            :if-exists if-exists
                            :element-type '(unsigned-byte 8))
      (zpng:start-png png stream)
      (dotimes (y height)
        (dotimes (x width)
          (zpng:write-pixel (aref image x y) png)))
      (zpng:finish-png png)))
  filename)
