;;;; concept graph ;;;;;;;;;;;;;;;;;;;;;

(in-package :db.agraph.user)  
(enable-!-reader)  
(enable-print-decoded t)
(register-namespace "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(register-namespace "owl" "http://www.w3.org/2002/07/owl#")

(defun find-owlsameas-conceptgraph (&key size)
  (let ((h (make-hash-table :test 'equalp :size (if (eq () size) 1000 size))))
    (iterate-cursor (tr (get-triples :p !owl:sameAs))
      (let ((s (subject tr)) (o (object tr)))
        (iterate-cursor (trs (get-triples :s s :p !rdf:type))
          (let ((stype (object trs)))
            (iterate-cursor (tro (get-triples :s o :p !rdf:type))	    
              (let ((key (list stype (object tro))))
                (let ((count (gethash key h)))
                  (setf (gethash key h)
                    (if (eq count nil) 1 (+ count 1))))))))))
    h))

(defun print-weighted-graph (h file)
  (with-open-file (*standard-output* file :direction :output :if-exists :supersede)
    (loop for key being the hash-key using (hash-value count) of h do
        (format t "~&~a ~a ~a" count (part->string (first key)) (part->string (second key))))))


;;;; domain graph ;;;;;;;;;;;;;;;;;;;;;;

(defun domain-of-uristring (s)
  (let ((re (load-time-value (compile-re "^<?(http|https|ftp)://([a-zA-Z0-9.:]+)\\S*"))))
    (multiple-value-bind (b url protocol host)
      (match-re re s)
      host)))

(defun domain-of-triple (tr)
  (let ((g (graph tr)))
    (if (eq nil g)
      nil
      (domain-of-uristring (part->string g)))))

(defun domain-of-resource (r)
  (domain-of-uristring (part->string r)))

(defun find-owlsameas-domaingraph (
    &key (undirected nil)
         (noselfedges nil)
         (size 1000))
  (let ((h (make-hash-table :test 'equalp :size size)))
    (iterate-cursor (tr (get-triples :p !owl:sameAs))
      (let (
          (sd (domain-of-resource (subject tr)))
	  (od (domain-of-resource (object tr))))
	(if (and
	    (not (eq nil sd))
	    (not (eq nil od))
	    (not (and noselfedges (eql sd od))))
          (let ((key (if (and undirected (eq 0 (string< od sd)))
	      (list od sd)
	      (list sd od))))
            (let ((count (gethash key h)))
              (setf (gethash key h)
                (if (eq count nil) 1 (+ count 1))))))))
    h))

(defun print-domain-graph (h file)
  (with-open-file (*standard-output* file :direction :output :if-exists :supersede)
    (loop for key being the hash-key using (hash-value count) of h do
        (format t "~&~a ~a ~a" count (first key) (second key)))))

