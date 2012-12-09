(in-package :bel-http)

;;; Functions to send an http response

(defun write-status-line (socket response)
  (declare (type http-response response))
  (socket-send socket
	       (format nil
		       "~A ~A ~A~A"
		       (http-response-http-version response)
		       (http-response-status-code response)
		       (http-response-reason-phrase response)
		       *line-ending*)
	       nil))

(defun write-header-fields (socket response)
  (declare (type http-response response))
  (maphash #'(lambda (key val)
	       (socket-send socket
			    (format nil "~A: ~A~A"
				    (string-capitalize
				     (string-downcase (symbol-name key)))
				    val
				    *line-ending*)
			    nil))
	   (http-response-header-fields response)))

(defun send-header (connection response)
  (declare (type http-response response))
  (write-status-line connection response)
  (write-header-fields connection response)
  (socket-send connection *line-ending* nil))

(defun copy-stream-to-socket (source socket)
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
	(pos))
    (loop until (= (setf pos (read-sequence buffer source)) 0) do
	 (socket-send socket buffer nil))))

;;; send-to-socket implementations
(defmethod send-to-socket (connection (content sequence))
  (socket-send connection content nil))

(defmethod send-to-socket (connection (content stream))
  (copy-stream-to-socket content connection))

(defmethod send-to-socket (connection (content null))) ; do nothing if content is null


;;; content-length implementations
(defmethod content-length ((content sequence))
  (length content))

(defmethod content-length ((content stream))
  (file-length content))

(defmethod content-length ((content null))
  0)

(defun send-body (connection response)
  (send-to-socket connection (http-response-body response)))

(export 'send-response)
(defun send-response (connection response)
  "writes a http-response to stream, will set the content length based on the length of body"
  (declare (type http-response response))
  (with-slots (header-fields body) response
    (setf (gethash :content-length header-fields) (content-length body))
    (send-header connection response)
    (send-body connection response)))
