<<<<<<< HEAD
(defun hashplusone (h k)
  (let ((count (gethash k h)))
    (setf (gethash k h)
      (if (eq count nil) 1 (+ count 1)))))


;;;; types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-type-distribution (&key (size 1000))
  (let ((dist (make-upi-hash-table :size size)))
    (iterate-cursor (tr (get-triples :p !rdf:type))
      (hashplusone dist (object tr)))
    dist))

(defun print-upi-distribution (dist file)
  (with-open-file (*standard-output* file :direction :output :if-exists :supersede)
    (maphash (lambda (k count)
        (format t "~&~a ~a" count (part->string k)))
      dist)))


=======
>>>>>>> a8a2951701977b44674d2c9bef009e73a277f99e
;;;; concept graph ;;;;;;;;;;;;;;;;;;;;;

(in-package :db.agraph.user)  
(enable-!-reader)  
(enable-print-decoded t)
(register-namespace "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
(register-namespace "owl" "http://www.w3.org/2002/07/owl#")

(defun find-owlsameas-conceptgraph (&key (size 1000))
  (let ((h (make-hash-table :test 'equalp :size size)))
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


<<<<<<< HEAD
;;;; "weak" concept graph ;;;;;;;;;;;;;;

(defun find-weak-concept-graph (b)
  (let ((g (make-hash-table :test 'equalp)))
    (select-distinct (?x ?c1 ?c2)
      (q ?x !rdf:type ?c1)
      (q ?x !rdf:type ?c2)
      (lisp ?b
        (let (
            (i1 (upi-to-id *atids* ?c1))
            (i2 (upi-to-id *atids* ?c2)))
          (if (not (eq i1 i2))
	    (let ((k (if (< i1 i2) (list i1 i2) (list i2 i1))))
	      (hashplusone g k))))))
    g))

(defun all-types (&key (size 1000))
  (let (( h (make-upi-hash-table :size size)))
    (iterate-cursor (tr (get-triples :p !rdf:type))
        (setf (gethash (object tr) h) t))
  h))

;; -> set of all subjects of rdf:type statements
(defun all-typed-resources (&key (size 1000))
  (let ((h (make-upi-hash-table :size size)))
    (iterate-cursor (tr (get-triples :p !rdf:type))
      (let ((s (subject tr)))
        (setf (gethash s h) t)))
    h))

(defun find-type-overlap (resources)
  (let ((overlap (make-hash-table)))
    (maphash (lambda (s v)
      (defparameter types ())
      (iterate-cursor (tr (get-triples :s s :p !rdf:type))
        (pushnew (object tr) types))
      (loop for t1 in types
        do (loop for t2 in types
          do (if (not (eq t1 t2))
	    ;;(let ((key (
            (format t "~& ~a ~a" t1 t2)
	    ))))
      resources)
    overlap))


=======
>>>>>>> a8a2951701977b44674d2c9bef009e73a277f99e
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

<<<<<<< HEAD

;;;; in-degree distribution ;;;;;;;;;;;;;;;;;;;;;;

(defun find-owlsameas-indegree (&key (size 1000))
  (let ((h (make-upi-hash-table :size size)))
    (iterate-cursor (tr (get-triples :p !owl:sameAs))
      (let ((key (object tr)))	    
        (let ((count (gethash key h)))
          (setf (gethash key h)
            (if (eq count nil) 1 (+ count 1))))))
    (let ((dist (make-hash-table)))
      (maphash (lambda (k v)
        (let ((degree (gethash v dist)))
          (setf (gethash v dist)
            (if (eq degree nil) 1 (+ degree 1)))))
        h)
      dist)))

(defun print-distribution (dist file)
  (with-open-file (*standard-output* file :direction :output :if-exists :supersede)
    (loop for key being the hash-key using (hash-value count) of dist do
        (format t "~&~a ~a" key count))))


;;;; export ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun output-owlsameas-pairs (file)
  (with-open-file (*standard-output* file :direction :output :if-exists :supersede)
    (iterate-cursor (tr (get-triples :p !owl:sameAs))
      (let ((s (part->value (subject tr))) (o (part->value (object tr))))
        (format t "~& ~a ~a" s o)))))

(defun is-uri (u)
  (multiple-value-bind (v c)
    (upi->value u)
    (eq 0 c)))

(defun upi-to-id (h u)
  (let ((v (gethash u h)))
    (if (eq nil v)
      (setf (gethash u h) (hash-table-count *ids*))
      v)))

(defun output-owlsameas-pairs-simple (file)
  (with-open-file (*standard-output* file :direction :output :if-exists :supersede)
    (defvar *ids* (make-upi-hash-table))
    (clrhash *ids*)
    (iterate-cursor (tr (get-triples :p !owl:sameAs))
      (let ((s (subject tr)) (o (object tr)))
        (if (and (is-uri s) (is-uri o))
          (format t "~& ~a ~a" (upi-to-id *ids* s) (upi-to-id *ids* o)))))))


;;;; R ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vector-to-dist (v)
  (let ((d (make-hash-table)))
    (maphash (lambda (k c)
        (hashplusone d c))
      v)
  d))

(defun print-hash-table (h f)
  (with-open-file (*standard-output* f :direction :output :if-exists :supersede)
    (maphash (lambda (k v)
        (format t "~&~a ~a" k v))
      h)))
      
(defun print-hash-values (h f)
  (with-open-file (*standard-output* f :direction :output :if-exists :supersede)
    (maphash (lambda (k v)
        (format t "~&~a" v))
      h)))

(defun vector-to-ids (v)
  (let ((d (make-upi-hash-table)))
    (maphash (lambda (k x)
        (upi-to-id d k))
      v)
  d))

















=======
>>>>>>> a8a2951701977b44674d2c9bef009e73a277f99e
