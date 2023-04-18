(in-package #:png)

#+cffi-features:darwin
(push #p"/usr/X11/lib/" cffi:*foreign-library-directories*)

#+cffi-features:darwin
(push #p"/opt/local/lib/" cffi:*foreign-library-directories*)

(define-foreign-library libpng
  (:darwin "libpng.dylib")
  (t (:default "libpng")))

(use-foreign-library libpng)

(defparameter +png-libpng-ver-string+ (get-png-libpng-ver-string))

;;; Foreign function definitions.

(defcfun "png_access_version_number" :uint32)

(declaim (inline png-create-read-struct))
(defcfun "png_create_read_struct" :pointer
  (user-png-ver :string)
  (error-ptr :pointer)
  (error-fn :pointer)
  (warn-fn :pointer))

(declaim (inline png-destroy-read-struct))
(defcfun "png_destroy_read_struct" :void
  (png-ptr-ptr :pointer)
  (info-ptr-ptr :pointer)
  (end-info-ptr-ptr :pointer))

(declaim (inline png-create-write-struct))
(defcfun "png_create_write_struct" :pointer
  (user-png-ver :string)
  (error-ptr :pointer)
  (error-fn :pointer)
  (warn-fn :pointer))

(declaim (inline png-destroy-write-struct))
(defcfun "png_destroy_write_struct" :void
  (png-ptr-ptr :pointer)
  (info-ptr-ptr :pointer))

(declaim (inline png-create-info-struct))
(defcfun "png_create_info_struct" :pointer
  (png-ptr :pointer))

(declaim (inline png-destroy-info-struct))
(defcfun "png_destroy_info_struct" :void
  (png-ptr :pointer)
  (info-ptr-ptr :pointer))

(declaim (inline png-init-io))
(defcfun "png_init_io" :void
  (png-ptr :pointer)
  (file :pointer))

(declaim (inline png-set-read-fn))
(defcfun "png_set_read_fn" :void
  (png-ptr :pointer)
  (io-ptr :pointer)
  (read-data-fn :pointer))

(declaim (inline png-set-write-fn))
(defcfun "png_set_write_fn" :void
  (png-ptr :pointer)
  (io-ptr :pointer)
  (write-data-fn :pointer)
  (output-flush-fn :pointer))

(declaim (inline png-get-io-ptr))
(defcfun "png_get_io_ptr" :pointer
  (png-ptr :pointer))

(declaim (inline png-read-info))
(defcfun "png_read_info" :void
  (png-ptr :pointer)
  (info-ptr :pointer))

(declaim (inline png-read-png))
(defcfun "png_read_png" :void
  (png-ptr :pointer)
  (info-ptr :pointer)
  (png-transforms :int)
  (params :pointer))

(declaim (inline png-get-ihdr))
(defcfun "png_get_IHDR" :uint32
  (png-ptr :pointer)
  (info-ptr :pointer)
  (width-uint32-ptr :pointer)
  (height-uint32-ptr :pointer)
  (bit-depth-int-ptr :pointer)
  (color-type-int-ptr :pointer)
  (interlace-type-int-ptr :pointer)
  (compression-type-int-ptr :pointer)
  (filter-type-int-ptr :pointer))

(declaim (inline png-set-ihdr))
(defcfun "png_set_IHDR" :void
  (png-ptr :pointer)
  (info-ptr :pointer)
  (width :uint32)
  (height :uint32)
  (bit-depth :int)
  (color-type :int)
  (interlace-type :int)
  (compression-type :int)
  (filter-type :int))

(declaim (inline png-set-bgr))
(defcfun "png_set_bgr" :void
  (png-ptr :pointer))

(declaim (inline png-set-palette-to-rgb))
(defcfun "png_set_palette_to_rgb" :void
  (png-ptr :pointer))

(declaim (inline png-set-expand-gray-1-2-4-to-8))
(defcfun "png_set_expand_gray_1_2_4_to_8" :void
  (png-ptr :pointer))

(declaim (inline png-set-expand))
(defcfun "png_set_expand" :void
  (png-ptr :pointer))

(declaim (inline png-get-valid))
(defcfun "png_get_valid" :uint32
  (png-ptr :pointer)
  (info-ptr :pointer)
  (flag :uint32))

(declaim (inline png-set-trns-to-alpha))
(defcfun "png_set_tRNS_to_alpha" :void
  (png-ptr :pointer))

(declaim (inline png-set-strip-16))
(defcfun "png_set_strip_16" :void
  (png-ptr :pointer))

(declaim (inline png-set-strip-alpha))
(defcfun "png_set_strip_alpha" :void
  (png-ptr :pointer))

(declaim (inline png-set-swap))
(defcfun "png_set_swap" :void
  (png-ptr :pointer))

(declaim (inline png-get-rows))
(defcfun "png_get_rows" :pointer
  (png-ptr :pointer)
  (info-ptr :pointer))

(declaim (inline png-set-rows))
(defcfun "png_set_rows" :void
  (png-ptr :pointer)
  (info-ptr :pointer)
  (row-pointers :pointer))

(declaim (inline png-read-image))
(defcfun "png_read_image" :void
  (png-ptr :pointer)
  (row-pointers :pointer))

(declaim (inline png-write-png))
(defcfun "png_write_png" :void
  (png-ptr :pointer)
  (info-ptr :pointer)
  (transforms :int)
  (params :pointer))

(declaim (inline memcpy))
(defcfun "memcpy" :pointer
  (dest :pointer)
  (source :pointer)
  (n size))

(defcstruct (%png-color :class png-color-type)
  (red png-byte)
  (green png-byte)
  (blue png-byte))

(defstruct png-color
  (red 0 :type (unsigned-byte 8))
  (green 0 :type (unsigned-byte 8))
  (blue 0 :type (unsigned-byte 8)))

(defmethod expand-into-foreign-memory (object (type png-color-type) pointer)
  `(with-foreign-slots ((red green blue) ,pointer (:struct %png-color))
     (setf red (png-color-red ,object)
           green (png-color-green ,object)
           blue (png-color-blue ,object))))

(defmethod translate-into-foreign-memory (object (type png-color-type) pointer)
  (with-foreign-slots ((red green blue) pointer (:struct %png-color))
    (setf red (png-color-red object)
          green (png-color-green object)
          blue (png-color-blue object))))

(defmethod expand-from-foreign (pointer (type png-color-type))
  `(with-foreign-slots ((red green blue) ,pointer (:struct %png-color))
     (make-png-color :red red :green green :blue blue)))

(defmethod translate-from-foreign (pointer (type png-color-type))
  (with-foreign-slots ((red green blue) pointer (:struct %png-color))
    (make-png-color :red red :green green :blue blue)))

(defcstruct (%png-color-8 :class png-color-8-type)
  (red png-byte)
  (green png-byte)
  (blue png-byte)
  (gray png-byte)
  (alpha png-byte))

(defstruct png-color-8
  (red 0 :type (unsigned-byte 16))
  (green 0 :type (unsigned-byte 16))
  (blue 0 :type (unsigned-byte 16))
  (gray 0 :type (unsigned-byte 16))
  (alpha 0 :type (unsigned-byte 16)))

(defmethod expand-into-foreign-memory ((object png-color-8) (type png-color-8-type) pointer)
  `(with-foreign-slots ((red green blue gray alpha) ,pointer (:struct %png-color-8))
     (setf alpha (png-color-8-alpha ,object)
           red (png-color-8-red ,object)
           green (png-color-8-green ,object)
           blue (png-color-8-blue ,object)
           gray (png-color-8-gray ,object))))

(defmethod translate-into-foreign-memory ((object png-color-8) (type png-color-8-type) pointer)
  (with-foreign-slots ((red green blue gray alpha) pointer (:struct %png-color-8))
    (setf alpha (png-color-8-alpha object)
          red (png-color-8-red object)
          green (png-color-8-green object)
          blue (png-color-8-blue object)
          gray (png-color-8-gray object))))

(defmethod expand-from-foreign (pointer (type png-color-8-type))
  `(with-foreign-slots ((red green blue gray alpha) ,pointer (:struct %png-color-8))
     (make-png-color-8 :alpha alpha :red red :green green :blue blue :gray gray)))

(defmethod translate-from-foreign (pointer (type png-color-8-type))
  (with-foreign-slots ((red green blue gray alpha) pointer (:struct %png-color-8))
    (make-png-color-8 :alpha alpha :red red :green green :blue blue :gray gray)))

(defcstruct (%png-color-16 :class png-color-16-type)
  (index png-byte)
  (red :uint16)
  (green :uint16)
  (blue :uint16)
  (gray :uint16))

(defstruct png-color-16
  (index 0 :type (unsigned-byte 8))
  (red 0 :type (unsigned-byte 16))
  (green 0 :type (unsigned-byte 16))
  (blue 0 :type (unsigned-byte 16))
  (gray 0 :type (unsigned-byte 16)))

(defmethod expand-into-foreign-memory ((object png-color-16) (type png-color-16-type) pointer)
  `(with-foreign-slots ((index red green blue gray) ,pointer (:struct %png-color-16))
     (setf index (png-color-16-index ,object)
           red (png-color-16-red ,object)
           green (png-color-16-green ,object)
           blue (png-color-16-blue ,object)
           gray (png-color-16-gray ,object))))

(defmethod translate-into-foreign-memory ((object png-color-16) (type png-color-16-type) pointer)
  (with-foreign-slots ((index red green blue gray) pointer (:struct %png-color-16))
    (setf index (png-color-16-index object)
          red (png-color-16-red object)
          green (png-color-16-green object)
          blue (png-color-16-blue object)
          gray (png-color-16-gray object))))

(defmethod expand-from-foreign (pointer (type png-color-16-type))
  `(with-foreign-slots ((index red green blue gray) ,pointer (:struct %png-color-16))
     (make-png-color-16 :index index :red red :green green :blue blue :gray gray)))

(defmethod translate-from-foreign (pointer (type png-color-16-type))
  (with-foreign-slots ((index red green blue gray) pointer (:struct %png-color-16))
    (make-png-color-16 :index index :red red :green green :blue blue :gray gray)))

(declaim (inline png-set-compression-level))
(defcfun "png_set_compression_level" :void
  (png-ptr :pointer)
  (level :int))

(declaim (inline png-set-plte))
(defcfun "png_set_PLTE" :void
  (png-ptr :pointer)
  (info-ptr :pointer)
  (palette :pointer)
  (num-palette :int))

(declaim (inline png-set-trns))
(defcfun "png_set_tRNS" :void
  (png-ptr :pointer)
  (info-ptr :pointer)
  (trans-alpha :pointer)
  (num-trans :int)
  (trans-color :pointer))

(declaim (inline png-set-sbit))
(defcfun "png_set_sBIT" :void
  (png-ptr :pointer)
  (info-ptr :pointer)
  (sig-bit :pointer))


;;; Input/output.

(defvar *stream*)

(defvar *buffer*)

(defun ensure-buffer-sufficient (needed)
  (when (< (length *buffer*) needed)
    (let ((new-length (length *buffer*)))
      (loop while (< new-length needed)
	 do (setf new-length (* 2 new-length)))
      (setf *buffer* (make-shareable-byte-vector new-length)))))

(defcallback user-read-data :void ((png-ptr :pointer) (data :pointer)
				   (length png-size))
  (declare (ignore png-ptr))
  (ensure-buffer-sufficient length)
  (let ((bytes-read (read-sequence *buffer* *stream* :start 0 :end length)))
    (unless (= bytes-read length)
      (error "Expected to read ~D bytes, but only read ~D." length
	     bytes-read)))
  (with-pointer-to-vector-data (buffer-ptr *buffer*)
    (memcpy data buffer-ptr length)))

(defcallback user-write-data :void ((png-ptr :pointer) (data :pointer)
				    (length png-size))
  (declare (ignore png-ptr))
  (ensure-buffer-sufficient length)
  (with-pointer-to-vector-data (buffer-ptr *buffer*)
    (memcpy buffer-ptr data length))
  (write-sequence *buffer* *stream* :start 0 :end length))

(defcallback user-flush-data :void ((png-ptr :pointer))
  (declare (ignore png-ptr)))


;;; Error handling.

(defcallback error-fn :void ((png-structp :pointer) (message :string))
  (declare (ignore png-structp))
  (error message))

(defcallback warn-fn :void ((png-structp :pointer) (message :string))
  (declare (ignore png-structp))
  (error message))


;;; Encode and decode PNG files.

(defmacro with-png-struct ((var &key (direction :input)) &body body)
  (let ((pointer (gensym "POINTER")))
    `(let ((,var (,(ecase direction
			  (:input 'png-create-read-struct)
			  (:output 'png-create-write-struct))
		   +png-libpng-ver-string+ (null-pointer)
		   (callback error-fn) (callback warn-fn)))
	   (*buffer* (make-shareable-byte-vector 1024)))
       (when (null-pointer-p ,var)
	 (error "Failed to allocate PNG write struct."))
       (unwind-protect (progn ,@body)
	 (with-foreign-pointer (,pointer ,(foreign-type-size :pointer))
	   (setf (mem-ref ,pointer :pointer) ,var)
	   ,(ecase direction
		   (:input `(png-destroy-read-struct ,pointer (null-pointer)
						     (null-pointer)))
		   (:output `(png-destroy-write-struct ,pointer
						       (null-pointer)))))))))

(defmacro with-png-info-struct ((var png-struct initform) &body body)
  (let ((pointer (gensym "POINTER")))
    `(let ((,var ,initform))
       (when (null-pointer-p ,var)
	 (error "Failed to allocate PNG info struct."))
       (unwind-protect (progn ,@body)
	 (with-foreign-pointer (,pointer (foreign-type-size :pointer))
	   (setf (mem-ref ,pointer :pointer) ,var)
	   (png-destroy-info-struct ,png-struct ,pointer))))))

(defun get-ihdr (png-ptr info-ptr)
  (with-foreign-pointer (width (foreign-type-size :uint32))
    (with-foreign-pointer (height (foreign-type-size :uint32))
      (with-foreign-pointer (bit-depth (foreign-type-size :int))
	(with-foreign-pointer (color-type (foreign-type-size :int))
	  (png-get-ihdr png-ptr info-ptr width height bit-depth
			color-type (null-pointer) (null-pointer)
			(null-pointer))
	  (values (mem-ref width :uint32) (mem-ref height :uint32)
		  (mem-ref bit-depth :int) (mem-ref color-type :int)))))))

(declaim (ftype (function (image) (integer 1 2)) bytes-per-pixel))
(defun bytes-per-pixel (image)
  (ecase (image-bit-depth image)
    (16 2)
    (8 1)))

(defmacro with-row-pointers ((rows-ptr image)
			     &body body)
  (let ((row-pointers (gensym "ROW-POINTERS"))
	(raw-data (gensym "RAW-DATA"))
	(i (gensym "I")))
    `(let ((,row-pointers (make-shareable-byte-vector (* (image-height ,image) (foreign-type-size :pointer)))))
       (with-pointer-to-vector-data (,rows-ptr ,row-pointers)
	 (with-pointer-to-array-data (,raw-data ,image)
	   (dotimes (,i (image-height ,image))
	     (setf (mem-aref ,rows-ptr :pointer ,i)
		   (inc-pointer ,raw-data (* ,i (image-width ,image)
					     (image-channels ,image)
					     (bytes-per-pixel ,image)))))
	   ,@body)))))

(defun grayp (color-type)
  (zerop (logand color-type (lognot +png-color-mask-alpha+))))

(defun decode (input &key swapbgr preserve-alpha)
  "Reads an image in PNG format from input and returns an array of
type IMAGE.  If the bit depth of the PNG file is less than or equal to
8, an 8-BIT-IMAGE will be returned; otherwise, a 16-BIT-IMAGE will be
returned.

Applications that would like to receive images of consistent bit
depth (rather than 8 or 16 depending on the PNG file) can apply the
function 8-BIT-IMAGE or the function 16-BIT-IMAGE to the result of
DECODE.

Bit depths less than 8 will be converted to 8 bits when read, and bit
depths between 8 and 16 bits will be converted to 16 bits.  As an
example, 2-bit PNG files contain only the pixel values 0, 1, 2, and 3.
These will be converted to 0, 85, 170, and 255, respectively, in order
to fill the dynamic range of the 8-bit image that is returned.

Swaps blue and red if SWAPBGR set.

Strips alpha channel unless PRESERVE-ALPHA is set.

Signals an error if reading the image fails."
  (with-png-struct (png-ptr :direction :input)
    (with-png-info-struct (info-ptr png-ptr (png-create-info-struct png-ptr))
      (with-png-info-struct (end-ptr png-ptr (png-create-info-struct png-ptr))
	(let ((*stream* input))
	  (png-set-read-fn png-ptr (null-pointer) (callback user-read-data))
	  (png-read-info png-ptr info-ptr)
	  (multiple-value-bind (width height bit-depth color-type)
	      (get-ihdr png-ptr info-ptr)
	    (when (= color-type +png-color-type-palette+)
	      (png-set-palette-to-rgb png-ptr))
	    (when (grayp color-type)
	      ;; png-set-expand-gray-1-2-4-to-8 did nothing on CCL
	      ;; DarwinPPC, but png-set-expand seems to work.
	      (png-set-expand png-ptr))
	    #+little-endian
	    (when (= bit-depth 16)
	      (png-set-swap png-ptr))
	    (unless (or preserve-alpha
			(zerop (logand color-type +png-color-mask-alpha+)))
	      (png-set-strip-alpha png-ptr))
        (when swapBGR
          (png-set-bgr png-ptr))
	(let* ((alphas (if (and preserve-alpha
				(plusp (logand color-type
					       +png-color-mask-alpha+)))
			   1 0))
	       (image (make-image height width
				  (+ (if (grayp color-type) 1 3) alphas)
				  (if (= 16 bit-depth) 16 8))))
	  (with-row-pointers (row-pointers image)
	    (png-set-rows png-ptr info-ptr row-pointers)
	    (png-read-image png-ptr row-pointers))
	  image)))))))

(defun decode-file (pathname &key swapbgr)
  (with-open-file (input pathname :element-type '(unsigned-byte 8))
    (decode input :swapbgr swapbgr)))

(defun encode (image output &key swapbgr)
  "Writes IMAGE in PNG format to OUTPUT.  The current version always
writes an 8-bit PNG file if image is an 8-BIT-IMAGE and a 16-bit PNG
file if image is an 16-BIT-IMAGE.  Future versions may write PNG files
of lower bit depths than IMAGE when the least significant bits may be
trimmed without loss of precision.

Signals an error if writing the image fails."
  (check-type image (or grayscale-image rgb-image))
  (with-png-struct (png-ptr :direction :output)
    (with-png-info-struct (info-ptr png-ptr (png-create-info-struct png-ptr))
      (let ((*stream* output))
	(png-set-write-fn png-ptr (null-pointer) (callback user-write-data)
			  (callback user-flush-data))
	(png-set-ihdr png-ptr info-ptr (image-width image) (image-height image)
		      (image-bit-depth image)
                      (ecase (image-channels image)
                        (1 +png-color-type-gray+)
                        (2 +png-color-type-gray-alpha+)
                        (3 +png-color-type-rgb+)
                        (4 +png-color-type-rgb-alpha+))
		      +png-interlace-none+ +png-compression-type-default+
		      +png-filter-type-default+)
	(when swapBGR
	  (png-set-bgr png-ptr))
	(with-row-pointers (row-pointers image)
	  (png-set-rows png-ptr info-ptr row-pointers)
	  (png-write-png png-ptr info-ptr
			 #+little-endian +png-transform-swap-endian+
			 #-little-endian +png-transform-identity+
			 (null-pointer))))))
  t)

(defun encode-file (image pathname &key swapbgr)
  (with-open-file (output pathname :element-type '(unsigned-byte 8)
			  :direction :output :if-exists :supersede)
    (encode image output :swapbgr swapbgr)))
