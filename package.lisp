(declaim (optimize (speed 3)))

(defpackage :bel-http
  (:use :cl :sb-bsd-sockets :bel-utils)
  (:export

   ;declarations symbols
   :http-request ;http-request symbols
   :make-http-request
   :http-request-p
   :http-request-method
   :http-request-request-uri
   :http-request-http-version
   :http-request-header-fields
   :http-request-body
   :method
   :request-uri
   :http-version
   :header-fields
   :body
   :http-response ;http-response symbols 
   :make-http-response
   :http-response-p
   :http-response-http-version
   :http-response-status-code
   :http-response-reason-phrase
   :http-response-header-fields
   :http-response-body
   :http-version
   :status-code
   :reason-phrase
   :header-fields
   :body

   ; receive-request symbols
   :receive-request 

   ; send-response symbols
   :send-response)) 