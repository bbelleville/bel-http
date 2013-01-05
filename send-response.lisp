(in-package :bel-http)

;;; Functions to send an http response

(defun write-status-line (out-stream response)
  (declare (type http-response response)
	   (type stream out-stream))
  (format out-stream
	  "~A ~A ~A~A"
	  (http-response-http-version response)
	  (http-response-status-code response)
	  (http-response-reason-phrase response)
	  *line-ending*))

(defun write-header-fields (out-stream response)
  (declare (type http-response response)
	   (type stream out-stream))
  (maphash #'(lambda (key val)
	       (format out-stream "~A: ~A~A"
		       (string-capitalize
			(string-downcase (symbol-name key)))
		       val
		       *line-ending*))
	   (http-response-header-fields response)))

(defun send-header (out-stream response)
  (declare (type http-response response)
	   (type stream out-stream))
  (write-status-line out-stream response)
  (write-header-fields out-stream response)
  (write-sequence *line-ending* out-stream))

(defun copy-stream-to-stream (source dest)
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
	(pos))
    (loop until (= (setf pos (read-sequence buffer source)) 0) do
	 (write-sequence buffer dest :end pos))))

;;; send-to-stream implementations
(defmethod send-to-stream (out-stream (content sequence))
  (declare (type stream out-stream)
	   (type sequence content))
  (write-sequence content out-stream))

(defmethod send-to-stream (out-stream (content stream))
  (declare (type stream out-stream content))
  (copy-stream-to-stream content out-stream))

(defmethod send-to-stream (connection (content null))) ; do nothing if content is null


;;; content-length implementations
(defmethod content-length ((content sequence))
  (length content))

(defmethod content-length ((content stream))
  (file-length content))

(defmethod content-length ((content null))
  0)

(defun send-body (out-stream response)
  (send-to-stream out-stream (http-response-body response)))

(defun send-response (out-stream response)
  "writes a http-response to stream, will set the content length based on the length of body"
  (declare (type http-response response)
	   (type stream out-stream))
  (with-slots (header-fields body) response
    (setf (gethash :content-length header-fields) (content-length body))
    (send-header out-stream response)
    (send-body out-stream response)))
