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

;; UDP networking ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq *ta-socket* (socket:make-socket :type :datagram :format :text :local-port 9999))

;; This is the maximum payload of a UDP message.
(defparameter *ta-buflen* 65507)
(defparameter *ta-buffer* (make-array *ta-buflen* :element-type 'character))

(defun process-next-transaction ()
  (multiple-value-bind (b l) (socket:receive-from *ta-socket* *ta-buflen* :extract t :buffer *ta-buffer*)
    ;;(print b)
    (apply-rdf-transaction-standalone
      (parse-rdf-transaction-string b))
    (commit-triple-store)))

