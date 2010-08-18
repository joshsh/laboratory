;;;; GeoNames ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun load-geonames (filepath)
  (with-open-file (in filepath)
    (let ((c 0) (line nil))
      (while (setf line (read-line in nil nil))
        (when (zerop (mod (incf c) 10000))
	  (commit-triple-store)
	  (print c)
	  (print (triple-count)))
        (if (eq 0 (mod c 2))
	  ;;(format t "~a~%" line)
	  (load-rdf/xml-from-string line
	    :graph (resource "http://sws.geonames.org")))))))

(defun load-geonames-debug (filepath)
  (with-open-file (in filepath)
    (let ((c 0) (line nil))
      (while (setf line (read-line in nil nil))
        (if (> c 580000)
	  (print c)
          (if (eq 1 (mod c 2))
	    (let ()
	    (format t "~a~%" line)
	    (force-output)
	    (load-rdf/xml-from-string line
	      :graph (resource "http://sws.geonames.org")))))
	(incf c)))))

