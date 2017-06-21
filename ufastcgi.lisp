
(defpackage :fcgi
  (:use :common-lisp :cffi)
  (:export :accept))

(defvar *fcgi-web-server-addrs* nil)

(defconstant +fcgi-version-1+ 1)

(let ((fcgi-types
       #(nil
         begin-request
         abort-request
         end-request
         params
         stdin
         stdout
         stderr
         data
         get-values
         get-values-result
         unknown)))
  (defun fcgi-type-ub8 (type)
    (position type fcgi-types :test 'eq))
  (defun fcgi-ub8-type (ub8)
    (when (< ub8 (length fcgi-types))
      (svref fcgi-types ub8))))

;;  unsigned bytes

(deftype ub8 (&optional length) `(array (unsigned-byte 8) (,length)))

(defun ub8 (&optional (contents ()) (length (length contents)))
  (apply #'make-array length :element-type '(unsigned-byte 8)
         (when contents
           `(:initial-contents ,contents))))

;;  FastCGI record

(defstruct fcgi-record
  (header (ub8 `(,+fcgi-version-1+ 0 0 0 0 0 0 0)) :type (ub8 8))
  (content (ub8) :type ub8)
  (padding (ub8) :type ub8))

(defgeneric fcgi-record-version (record))
(defgeneric (setf fcgi-record-version) (value record))
(defgeneric fcgi-record-type (record))
(defgeneric (setf fcgi-record-type) (value record))
(defgeneric fcgi-record-request-id (record))
(defgeneric (setf fcgi-record-request-id) (value record))
(defgeneric fcgi-record-content-length (record))
(defgeneric (setf fcgi-record-content-length) (value record))
(defgeneric fcgi-record-padding-length (record))
(defgeneric (setf fcgi-record-padding-length) (value record))

(defmethod fcgi-record-version ((a array))
  (aref a 0))

(defmethod (setf fcgi-record-version) (value (a array))
  (setf (aref a 0) value))

(defmethod fcgi-record-type ((a array))
  (fcgi-ub8-type (aref a 1)))

(defmethod (setf fcgi-record-type) (value (a array))
  (setf (aref a 1) (fcgi-type-ub8 value)))

(defmethod fcgi-record-request-id ((a array))
  (logior (ash (aref a 2) 8)
          (aref a 3)))

(defmethod (setf fcgi-record-request-id) (value (a array))
  (setf (aref a 2) (logand #xFF (ash value -8))
        (aref a 3) (logand #xFF value)))

(defmethod fcgi-record-content-length ((a array))
  (logior (ash (aref a 4) 8)
          (aref a 5)))

(defmethod (setf fcgi-record-content-length) (value (a array))
  (setf (aref a 4) (logand #xFF (ash value -8))
        (aref a 5) (logand #xFF value)))

(defmethod fcgi-record-padding-length ((a array))
  (aref a 6))

(defmethod (setf fcgi-record-padding-length) (value (a array))
  (setf (aref a 6) value))

(defun fcgi-record (&key (version +fcgi-version-1+)
                      (type 0)
                      (request-id 0)
                      (content (ub8))
                      (content-length (length content))
                      (padding-length 0)
                      (padding (ub8 () padding-length)))
  (let ((header (ub8 () 8)))
    (setf (fcgi-record-version header) version
          (fcgi-record-type header) type
          (fcgi-record-request-id header) request-id
          (fcgi-record-content-length header) content-length
          (fcgi-record-padding-length header) padding-length)
    (make-fcgi-record :header header :content content :padding padding)))

(defmethod print-object ((obj fcgi-record) stream)
  (print `(fcgi-record :version ,(fcgi-record-version obj)
                       :type ,(fcgi-record-type obj)
                       :request-id ,(fcgi-record-request-id obj)
                       :content ,(fcgi-record-content obj)
                       :padding-length ,(fcgi-record-padding-length obj))))

(defmethod fcgi-record-version ((r fcgi-record))
  (fcgi-record-version (fcgi-record-header r)))

(defmethod (setf fcgi-record-version) (value (r fcgi-record))
  (setf (fcgi-record-version (fcgi-record-header r)) value))

(defmethod fcgi-record-type ((r fcgi-record))
  (fcgi-record-type (fcgi-record-header r)))

(defmethod (setf fcgi-record-type) (value (r fcgi-record))
  (setf (fcgi-record-type (fcgi-record-header r)) value))

(defmethod fcgi-record-request-id ((r fcgi-record))
  (fcgi-record-request-id (fcgi-record-header r)))

(defmethod (setf fcgi-record-request-id) (value (r fcgi-record))
  (setf (fcgi-record-request-id (fcgi-record-header r)) value))

(defmethod fcgi-record-content-length ((r fcgi-record))
  (fcgi-record-content-length (fcgi-record-header r)))

(defmethod (setf fcgi-record-content-length) (value (r fcgi-record))
  (setf (fcgi-record-content-length (fcgi-record-header r)) value))

(defmethod fcgi-record-padding-length ((r fcgi-record))
  (fcgi-record-padding-length (fcgi-record-header r)))

(defmethod (setf fcgi-record-padding-length) (value (r fcgi-record))
  (setf (fcgi-record-padding-length (fcgi-record-header r)) value))

(defun read-fcgi-record (stream)
  (when (open-stream-p stream)
    (let ((header (make-array 8 :element-type '(unsigned-byte 8))))
      (read-sequence header stream)
      (let ((content (ub8 () (fcgi-record-content-length header)))
            (padding (ub8 () (fcgi-record-padding-length header))))
        (read-sequence content stream)
        (read-sequence padding stream)
        (make-fcgi-record :header header
                          :content content
                          :padding padding)))))

(defun write-fcgi-record (record stream)
  (write-sequence (fcgi-record-header record) stream)
  (write-sequence (fcgi-record-content record) stream)
  (write-sequence (fcgi-record-padding record) stream)
  (force-output stream))

;;  Requests

(defvar *fcgi-requests*
  (make-hash-table :test 'eq))

;;  Name value pairs

(defun read-1-or-4-bytes (stream)
  (let ((b (read-byte stream)))
    (if (zerop (ash b -7))
        b
        (+ (ash (logand b #x7F) 24)
           (ash (read-byte stream) 16)
           (ash (read-byte stream) 8)
           (read-byte stream)))))

(defun read-name-value (stream)
  (let* ((name-length (read-1-or-4-bytes stream))
         (value-length (read-1-or-4-bytes stream))
         (name (make-array name-length :element-type '(unsigned-byte 8)))
         (value (make-array value-length :element-type '(unsigned-byte 8))))
    (read-sequence name stream)
    (read-sequence value stream)
    (cons name value)))

(defun write-1-or-4-bytes (value stream)
  (cond ((< value #x80)
         (write-byte value stream))
        (t
         (write-byte (logior #x80 (logand #x7F (ash value -24))) stream)
         (write-byte (logand #xFF (ash value -16)) stream)
         (write-byte (logand #xFF (ash value -8)) stream)
         (write-byte (logand #xFF value) stream)))
  (force-output stream))

(defun write-name-value (cons stream)
  (let ((name (car cons))
        (value (cdr cons)))
    (write-1-or-4-bytes (length name) stream)
    (write-1-or-4-bytes (length value) stream)
    (write-sequence name stream)
    (write-sequence value stream)))

;;  Variables

(defvar *fcgi-max-conn* 1)

(defvar *fcgi-max-reqs* 1)

(defvar *fcgi-mpxs-conns* 0)

(defun fcgi-unknown-type (type)
  (let ((header (make-array 8 :element-type '(unsigned-byte 8)))
        (content (make-array 8 :element-type '(unsigned-byte 8))))
    (setf (fcgi-record-version header) +fcgi-version-1+
          (fcgi-record-type header) 'unknown
          (fcgi-record-request-id header) 0
          (fcgi-record-content-length header) 8
          (fcgi-record-padding-length header) 0
          (aref content 0) type)
    (make-fcgi-record :header header :content content)))

;;  Sockets

(defcfun ("socket" c-socket) :int
  (domain :int)
  (type :int)
  (protocol :int))

(defcfun ("bind" c-bind) :int
  (sockfd :int)
  (addr :pointer)
  (addrlen :int))

(defcfun ("listen" c-listen) :int
  (sockfd :int)
  (backlog :int))

(defcfun ("accept" c-accept) :int
  (addr :pointer)
  (addrlen (:pointer :int)))

(defun socket 
(defun accept (

(defun server-on-socket (func bound-socket)
  (let* ((sock (usocket:socket-accept bound-socket
                                      :element-type '(unsigned-byte 8)))
         (stream (usocket:socket-stream sock)))
    (when sock
      (loop while (eq :read-write (usocket:socket-state sock))
         for record = (read-fcgi-record stream)
         do (when record
              (format t "~&~S~%" record)))
      (usocket:socket-close sock))
    (server-on-socket func bound-socket)))

(defvar *sockets* ())

(defun close-all-sockets ()
  (unless (endp *sockets*)
    (usocket:socket-close (pop *sockets*))
    (close-all-sockets)))

(defun socket-server (func &key
                             (inet-addr "127.0.0.1")
                             (port 9000))
  (let ((sock (usocket:socket-listen inet-addr port
                                     :reuse-address t :backlog 128)))
    (push sock *sockets*)
    (server-on-socket func sock)))

(trace socket-server usocket:socket-state)

(defun test-func (s)
  (format t "~S" (usocket:get-peer-address s)))

#+test
(close-all-sockets)

#+test
(socket-server 'test-func :port 4207)
