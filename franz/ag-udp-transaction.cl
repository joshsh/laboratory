;; depends on:
;;     ags-backend-minimal.cl
;;     xmltransaction.cl

(in-package :ags.backend)


;; RDF transactions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun apply-rdf-transaction-standalone (actions)
  (let ((bnodes (make-hash-table :test 'equal)))
    (flet ((finish (part)
             (etypecase part
               (keyword (default-graph-upi *db*)) ;; :null
               (null nil)
               (cons (or (gethash (second part) bnodes) ;; (:bnode "id")
                         (setf (gethash (second part) bnodes) (new-blank-node))))
               (triple-part part))))
      (dolist (action actions)
	;;(print action)
        (ecase (car action)
          (:add
	   ;;(print "add...")
           (destructuring-bind (subj pred obj . contexts) (mapcar #'finish (cdr action))
             (dolist (context (or contexts (list (default-graph-upi *db*))))
               (add-triple subj pred obj :g context))))
          (:remove
	   ;;(print "remove...")
           (destructuring-bind (subj pred obj . contexts) (mapcar #'finish (cdr action))
             (dolist (context (or contexts (list nil)))
               (delete-triples :s subj :p pred :o obj :g context))))
          (:remove-namespace
	   ;;(print "remove-namespace...")
           (let ((prefix (second action)))
             (remove-namespace prefix)))
          (:add-namespace
	   ;;(print "add-namespace...")
           (destructuring-bind (prefix name) (cdr action)
             (register-namespace prefix name :errorp nil)))
          (:clear-namespaces
	   ;;(print "clear-namespaces...")
           (clear-namespaces)))))))


;; UDP transaction streams ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the maximum payload of a UDP message.
(defparameter *receiver-buflen* 65507)

(defun create-receiver-socket (port)
  (socket:make-socket :type :datagram :format :text :local-port port))

(defun make-udp-buffer ()
    (make-array *receiver-buflen* :element-type 'character))
    
(defun process-next-transaction (socket buffer)
  (multiple-value-bind (b l)
    (socket:receive-from socket *receiver-buflen* :extract t :buffer buffer)
    ;;(print b)
    (apply-rdf-transaction-standalone
      ;;(handler-case
        (parse-rdf-transaction-string b)
      ;;  (net.xml.sax:sax-error (e)
      ;;  (format t "~&error: ~a when parsing ~a" e b)
      ;;  ))
	)
    ;;(commit-triple-store)
    ))

(defun receive-transactions (name socket trans-per-commit commits-per-batch)
  (let ((buffer (make-udp-buffer)))
  (loop (let ((start (get-internal-real-time)))
    (setf d 0)
    (loop for i from 1 to commits-per-batch do
      (loop for j from 1 to trans-per-commit do
        (handler-case
	  (process-next-transaction socket buffer)
	    (net.xml.sax:sax-error (e)
	      (format t "~&operation discarded due to error: ~a" e)
	      (setf d (+ d 1))))
	)
      (commit-triple-store))
    (let ((elapsed (- (get-internal-real-time) start))
        (count (* trans-per-commit commits-per-batch)))
      (format t "~&~a: ~a transactions (~a discarded) with ~a commits in ~ams (real time) --> ~a/s at ~a triples"
        name
        count
	d
	commits-per-batch
	elapsed
	(* 1000.0 (/ count elapsed))
	(triple-count)))
    ))))

(defun run-receiver-thread (name socket trans-per-commit commits-per-batch)
  (mp:process-run-function name
    (lambda ()
      (receive-transactions name socket trans-per-commit commits-per-batch))))

