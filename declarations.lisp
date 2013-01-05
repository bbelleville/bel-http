(in-package :bel-http)

(defparameter *line-ending* (format nil "~C~C" #\return #\newline))
(defparameter *white-space* (list #\space #\tab))

(defstruct http-request
  method
  request-uri
  http-version
  header-fields
  body)

(defstruct http-response
  http-version
  status-code
  reason-phrase
  header-fields
  body)

(defgeneric content-length (content)
  (:documentation "a method to determine the content length of different types of content that can be sent"))

(defgeneric send-to-stream (out-stream content)
  (:documentation "Send the content to the socket connection. To specialize on the different types of content"))

	      