(declaim (optimize (speed 3)))

(defpackage :bel-http
  (:use :cl :sb-bsd-sockets :bel-utils))