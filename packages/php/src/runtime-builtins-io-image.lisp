;;;; PHP image metadata builtins.

(in-package :cl-cc/php)

;;; ─── Image Metadata ─────────────────────────────────────────────────────────

(defun %php-image-type-info (image-type)
  "Return extension and MIME type for a PHP IMAGETYPE_* integer."
  (case (%php-to-integer image-type)
    (1 (values "gif" "image/gif"))
    (2 (values "jpeg" "image/jpeg"))
    (3 (values "png" "image/png"))
    (4 (values "swf" "application/x-shockwave-flash"))
    (5 (values "psd" "image/vnd.adobe.photoshop"))
    (6 (values "bmp" "image/bmp"))
    (7 (values "tiff" "image/tiff"))
    (8 (values "tiff" "image/tiff"))
    (9 (values "jpc" "application/octet-stream"))
    (10 (values "jp2" "image/jp2"))
    (11 (values "jpx" "application/octet-stream"))
    (12 (values "jb2" "application/octet-stream"))
    (13 (values "swc" "application/x-shockwave-flash"))
    (14 (values "iff" "image/iff"))
    (15 (values "wbmp" "image/vnd.wap.wbmp"))
    (16 (values "xbm" "image/xbm"))
    (17 (values "ico" "image/vnd.microsoft.icon"))
    (18 (values "webp" "image/webp"))
    (19 (values "avif" "image/avif"))
    (20 (values "heif" "image/heif"))
    (21 (values "svg" "image/svg+xml"))
    (otherwise (values nil nil))))

(defun %php-image-type-to-extension (image-type &optional (include-dot t))
  "PHP image_type_to_extension: map IMAGETYPE_* to an extension."
  (multiple-value-bind (extension mime)
      (%php-image-type-info image-type)
    (declare (ignore mime))
    (when extension
      (if (%php-truthy include-dot)
          (concatenate 'string "." extension)
          extension))))

(defun %php-image-type-to-mime-type (image-type)
  "PHP image_type_to_mime_type: map IMAGETYPE_* to a MIME type."
  (multiple-value-bind (extension mime)
      (%php-image-type-info image-type)
    (declare (ignore extension))
    mime))

(defun %php-read-file-bytes (filename &optional limit)
  (handler-case
      (with-open-file (stream (%php-path-string filename)
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (let* ((size (file-length stream))
               (count (if limit (min size limit) size))
               (data (make-array count :element-type '(unsigned-byte 8))))
          (read-sequence data stream)
          data))
    (error () nil)))

(defun %php-read-file-text-prefix (filename &optional (limit 65536))
  (handler-case
      (with-open-file (stream (%php-path-string filename)
                              :direction :input
                              :external-format :utf-8)
        (let* ((size (file-length stream))
               (count (min size limit))
               (data (make-string count)))
          (read-sequence data stream)
          data))
    (error () nil)))

(defun %php-bytes-prefix-p (bytes values &optional (start 0))
  (and bytes
       (<= (+ start (length values)) (length bytes))
       (loop for value in values
             for index from start
             always (= (aref bytes index) value))))

(defun %php-bytes-match-string-p (bytes start string)
  (and bytes
       (<= (+ start (length string)) (length bytes))
       (loop for index below (length string)
             always (= (aref bytes (+ start index))
                       (char-code (char string index))))))

(defun %php-bytes-ascii-string (bytes start end)
  (coerce (loop for index from start below (min end (length bytes))
                collect (code-char (aref bytes index)))
          'string))

(defun %php-u16-be (bytes start)
  (+ (ash (aref bytes start) 8)
     (aref bytes (1+ start))))

(defun %php-u16-le (bytes start)
  (+ (aref bytes start)
     (ash (aref bytes (1+ start)) 8)))

(defun %php-u32-be (bytes start)
  (+ (ash (aref bytes start) 24)
     (ash (aref bytes (+ start 1)) 16)
     (ash (aref bytes (+ start 2)) 8)
     (aref bytes (+ start 3))))

(defun %php-webp-bytes-p (bytes)
  (and bytes
       (<= 12 (length bytes))
       (%php-bytes-match-string-p bytes 0 "RIFF")
       (%php-bytes-match-string-p bytes 8 "WEBP")))

(defun %php-iso-bmff-brands (bytes)
  (when (and bytes
             (<= 12 (length bytes))
             (%php-bytes-match-string-p bytes 4 "ftyp"))
    (string-downcase
     (%php-bytes-ascii-string bytes 8 (min 128 (length bytes))))))

(defun %php-iso-bmff-brand-p (bytes brands)
  (let ((detected-brands (%php-iso-bmff-brands bytes)))
    (and detected-brands
         (some (lambda (brand)
                 (search brand detected-brands :test #'char=))
               brands))))

(defun %php-avif-bytes-p (bytes)
  (%php-iso-bmff-brand-p bytes '("avif" "avis")))

(defun %php-heif-bytes-p (bytes)
  (%php-iso-bmff-brand-p bytes '("heic" "heix" "hevc" "hevx" "heim" "heis")))

(defun %php-image-type-from-bytes (bytes)
  (cond
    ((or (%php-bytes-match-string-p bytes 0 "GIF87a")
         (%php-bytes-match-string-p bytes 0 "GIF89a"))
     1)
    ((%php-bytes-prefix-p bytes '(#xff #xd8 #xff)) 2)
    ((%php-bytes-prefix-p bytes '(#x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a)) 3)
    ((%php-webp-bytes-p bytes) 18)
    ((%php-avif-bytes-p bytes) 19)
    ((%php-heif-bytes-p bytes) 20)
    (t nil)))

(defun %php-svg-text-p (text)
  (and text (search "<svg" text :test #'char-equal)))

(defun %php-detect-image-type (filename)
  (let* ((bytes (%php-read-file-bytes filename 65536))
         (image-type (%php-image-type-from-bytes bytes)))
    (if image-type
        (values image-type bytes nil)
        (let ((text (%php-read-file-text-prefix filename)))
          (when (%php-svg-text-p text)
            (values 21 bytes text))))))

(defun %php-png-dimensions (bytes)
  (when (and bytes (<= 24 (length bytes)))
    (values (%php-u32-be bytes 16)
            (%php-u32-be bytes 20))))

(defun %php-gif-dimensions (bytes)
  (when (and bytes (<= 10 (length bytes)))
    (values (%php-u16-le bytes 6)
            (%php-u16-le bytes 8))))

(defun %php-jpeg-sof-marker-p (marker)
  (member marker '(#xc0 #xc1 #xc2 #xc3 #xc5 #xc6 #xc7 #xc9 #xca #xcb #xcd #xce #xcf)))

(defun %php-jpeg-dimensions (bytes)
  (when (and bytes (%php-bytes-prefix-p bytes '(#xff #xd8)))
    (let ((position 2)
          (size (length bytes)))
      (loop while (< (+ position 3) size)
            do (progn
                 (loop while (and (< position size)
                                  (/= (aref bytes position) #xff))
                       do (incf position))
                 (loop while (and (< position size)
                                  (= (aref bytes position) #xff))
                       do (incf position))
                 (when (>= position size)
                   (return-from %php-jpeg-dimensions nil))
                 (let ((marker (aref bytes position)))
                   (incf position)
                   (when (= marker #xd9)
                     (return-from %php-jpeg-dimensions nil))
                   (unless (member marker '(#xd8 #x01))
                     (when (>= (+ position 2) size)
                       (return-from %php-jpeg-dimensions nil))
                     (let ((segment-size (%php-u16-be bytes position)))
                       (when (< segment-size 2)
                         (return-from %php-jpeg-dimensions nil))
                       (when (and (%php-jpeg-sof-marker-p marker)
                                  (<= (+ position 7) size))
                         (return-from %php-jpeg-dimensions
                           (values (%php-u16-be bytes (+ position 5))
                                   (%php-u16-be bytes (+ position 3)))))
                       (when (= marker #xda)
                         (return-from %php-jpeg-dimensions nil))
                       (incf position segment-size)))))))))

(defun %php-svg-start-tag (text)
  (let ((start (and text (search "<svg" text :test #'char-equal))))
    (when start
      (let ((end (position #\> text :start start)))
        (subseq text start (or end (length text)))))))

(defun %php-svg-name-char-p (char)
  (or (alphanumericp char)
      (member char '(#\_ #\- #\:))))

(defun %php-xml-space-p (char)
  (member char '(#\Space #\Tab #\Newline #\Return #\Page)))

(defun %php-skip-xml-space (string position)
  (loop while (and (< position (length string))
                   (%php-xml-space-p (char string position)))
        do (incf position))
  position)

(defun %php-svg-attribute-value (tag name)
  (let ((position 0)
        (name-length (length name)))
    (loop
      (let ((found (search name tag :start2 position :test #'char-equal)))
        (unless found
          (return nil))
        (let* ((after-name (+ found name-length))
               (before-ok (or (= found 0)
                              (not (%php-svg-name-char-p (char tag (1- found))))))
               (after-ok (or (>= after-name (length tag))
                             (not (%php-svg-name-char-p (char tag after-name))))))
          (setf position after-name)
          (when (and before-ok after-ok)
            (let ((cursor (%php-skip-xml-space tag after-name)))
              (when (and (< cursor (length tag))
                         (char= (char tag cursor) #\=))
                (incf cursor)
                (setf cursor (%php-skip-xml-space tag cursor))
                (when (< cursor (length tag))
                  (let ((quote (char tag cursor)))
                    (if (member quote '(#\" #\'))
                        (let ((end (position quote tag :start (1+ cursor))))
                          (when end
                            (return (subseq tag (1+ cursor) end))))
                        (let ((end (or (position-if
                                        (lambda (char)
                                          (or (%php-xml-space-p char)
                                              (char= char #\>)
                                              (char= char #\/)))
                                        tag
                                        :start cursor)
                                       (length tag))))
                          (return (subseq tag cursor end))))))))))))))

(defun %php-svg-number-value (number-string)
  (handler-case
      (if (find #\. number-string)
          (let ((value (read-from-string number-string nil 0)))
            (if (and (numberp value)
                     (zerop (- value (round value))))
                (round value)
                value))
          (parse-integer number-string))
    (error () 0)))

(defun %php-svg-dimension (value)
  (let* ((trimmed (if value
                      (string-trim '(#\Space #\Tab #\Newline #\Return #\Page) value)
                      ""))
         (position 0)
         (seen-dot nil))
    (when (and (< position (length trimmed))
               (member (char trimmed position) '(#\+ #\-)))
      (incf position))
    (loop while (and (< position (length trimmed))
                     (or (digit-char-p (char trimmed position))
                         (and (char= (char trimmed position) #\.)
                              (not seen-dot))))
          do (when (char= (char trimmed position) #\.)
               (setf seen-dot t))
             (incf position))
    (let* ((number-string (subseq trimmed 0 position))
           (unit (string-trim '(#\Space #\Tab #\Newline #\Return #\Page)
                              (subseq trimmed position))))
      (values (if (plusp (length number-string))
                  (%php-svg-number-value number-string)
                  0)
              (if (plusp (length unit)) unit "px")))))

(defun %php-svg-dimensions (text)
  (let* ((tag (%php-svg-start-tag text))
         (width-value (and tag (%php-svg-attribute-value tag "width")))
         (height-value (and tag (%php-svg-attribute-value tag "height"))))
    (multiple-value-bind (width width-unit) (%php-svg-dimension width-value)
      (multiple-value-bind (height height-unit) (%php-svg-dimension height-value)
        (values width height width-unit height-unit)))))

(defun %php-image-dimensions-and-units (image-type bytes text)
  (multiple-value-bind (width height)
      (case image-type
        (1 (%php-gif-dimensions bytes))
        (2 (%php-jpeg-dimensions bytes))
        (3 (%php-png-dimensions bytes))
        (21 (return-from %php-image-dimensions-and-units
              (%php-svg-dimensions text)))
        (otherwise (values 0 0)))
    (values (or width 0) (or height 0) "px" "px")))

(defun %php-getimagesize (filename &optional image-info)
  (declare (ignore image-info))
  (multiple-value-bind (image-type bytes text) (%php-detect-image-type filename)
    (when image-type
      (multiple-value-bind (width height width-unit height-unit)
          (%php-image-dimensions-and-units image-type bytes text)
        (multiple-value-bind (extension mime) (%php-image-type-info image-type)
          (declare (ignore extension))
          (let ((result (%php-make-array)))
            (%php-array-set result 0 width)
            (%php-array-set result 1 height)
            (%php-array-set result 2 image-type)
            (%php-array-set result 3 (format nil "width=\"~A\" height=\"~A\"" width height))
            (%php-array-set result "mime" mime)
            (%php-array-set result "width_unit" width-unit)
            (%php-array-set result "height_unit" height-unit)
            result))))))

(defun %php-exif-imagetype (filename)
  (multiple-value-bind (image-type bytes text) (%php-detect-image-type filename)
    (declare (ignore bytes text))
    image-type))
