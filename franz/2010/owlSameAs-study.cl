
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

