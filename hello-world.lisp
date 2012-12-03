(use-package (list :sb-bsd-sockets :cl :bel-http))

(defun nslookup (hostname)
   (if hostname
       (host-ent-address (get-host-by-name hostname))
       nil))

(defun respond (server)
	     (let ((con (socket-make-stream (socket-accept cl-user::server)  :input t :output t)))
	       (parse-request-header con)
	       (send-response con 
			      (make-http-response 
			       :http-version "HTTP/1.0"
			       :status-code "200"
			       :reason-phrase "GreatSuccess"
			       :header-fields (let ((x (make-hash-table)))
						(setf (gethash "Server" x) "bell-http")
						x)
			       :body (format nil "hello world~%")))
	       (close con))) 

(defun main (port)
  (let ((server (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (unwind-protect
	 (progn 
	   (socket-bind server '(0 0 0 0) port)
	   (socket-listen server 5)
	   (loop
	      (handler-case
		  (respond server)
		(error () nil))))
      (when server (socket-close server)))))
  
