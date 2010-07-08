;;;; GeoNames ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-geonames (filepath)
  (with-open-file (in filepath)
    (let ((odd nil) (c 0) (line nil))
      (while (setf line (read-line in nil nil))
        (when (zerop (mod (incf c) 10000))
	  (commit-triple-store)
	  (print c))
        (if (eq 0 (mod c 2)
	    ;;(format t "~a~%" line)
	    (load-rdf/xml-from-string line
	      :graph (resource "http://sws.geonames.org"))))))
    (close in)))

