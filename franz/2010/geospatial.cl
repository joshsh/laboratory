(in-package :db.agraph.user)  
(enable-!-reader)
(register-namespace "pos" "http://www.w3.org/2003/01/geo/wgs84_pos#")
(register-namespace "geo" "http://franz.com/ns/allegrograph/3.0/geospatial/fn/")


;; geo indexing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun geo-setup ()
    ;; -> http://franz.com/ns/allegrograph/3.0/geospatial/spherical/miles/-180.0/180.0/-90.0/90.0/1.0
    (defparameter *lat-lon-1*   (register-latitude-striping-in-miles 1.0s0))
    ;; -> http://franz.com/ns/allegrograph/3.0/geospatial/spherical/miles/-180.0/180.0/-90.0/90.0/5.0
    (defparameter *lat-lon-5*   (register-latitude-striping-in-miles 5.0s0))
    ;; -> http://franz.com/ns/allegrograph/3.0/geospatial/spherical/miles/-180.0/180.0/-90.0/90.0/100.0
    (defparameter *lat-lon-100* (register-latitude-striping-in-miles 100.0s0))
    (defparameter *geo-properties*
        '((!pos:long :longitude)
          (!pos:lat :latitude)))
    (add-geospatial-subtype-to-db *lat-lon-1*)
    (add-geospatial-subtype-to-db *lat-lon-5*)
    (add-geospatial-subtype-to-db *lat-lon-100*))

(defun get-latitude (r)
    (let (default)
        (iterate-cursor (tr (get-triples :s r :p !pos:lat))
            (handler-case
	        (return-from get-latitude
	            (+ 0.0 (read-from-string (upi->value (object tr)))))
	        (type-error (e) ())
		(reader-error (e) ())))
    default))
(defun get-longitude (r)
    (let (default)
        (iterate-cursor (tr (get-triples :s r :p !pos:long))
            (handler-case
	        (return-from get-longitude
	            (+ 0.0 (read-from-string (upi->value (object tr)))))
	        (type-error (e) ())
		(reader-error (e) ())))
    default))

;; 
(defun normalize-latitude (lat)
    (if (eq nil lat) nil
        (if (< lat -90) nil
            (if (> lat 90) nil lat))))
	    
;; 
(defun normalize-longitude (lon)
    (if (eq nil lon) nil
        (if (< lon -360) nil
            (if (> lon 360) nil
	        (if (< lon -180) (+ 360 lon)
	            (if (> lon 180) (- 360 lon) lon))))))

;; geo-index a resource with valid pos:long and pos:lat values
(defun index-geopoint (geopoint)
    (let (
        (lat (normalize-latitude (get-latitude geopoint)))
	(lon (normalize-longitude (get-longitude geopoint))))
	(when (and (not (eq nil lat)) (not (eq nil lon)))
	    (with-temp-upi (upi)
	        (add-triple geopoint
	            !geo:isAt1
                    (longitude-latitude->upi *lat-lon-1* lon lat upi)
		    :g !geo:graph)
	        (add-triple geopoint
	            !geo:isAt5
                    (longitude-latitude->upi *lat-lon-5* lon lat upi)
		    :g !geo:graph)
                (add-triple geopoint
                    !geo:isAt100
                    (longitude-latitude->upi *lat-lon-100* lon lat upi)
		    :g !geo:graph)))))

(defun count-all-geopoints ()
    (defparameter *count* 0)
    (iterate-cursor (tr (get-triples :p !pos:lat))
        (let ((geopoint (subject tr)))
            (let (
	        (lat (normalize-latitude (get-latitude geopoint)))
	        (lon (normalize-longitude (get-longitude geopoint))))
	        (when (and (not (eq nil lat)) (not (eq nil lon)))
	            (setf *count* (+ 1 *count*))))))
    *count*)
	    
(defun index-all-geopoints ()
    (defparameter *geoindexing-in-progress* t)
    ;; Anything with a pos:lat is considered a geopoint
    (iterate-cursor (tr (get-triples :p !pos:lat))
        (index-geopoint (subject tr)))
    ;;(index-new-triples)
    (defparameter *geoindexing-in-progress* nil))
    
(defun delete-all-geo-triples ()
    (delete-triples :g !geo:graph))

;; Note: there are three triples per geopoint
(defun count-geo-triples ()
    (count-cursor (get-triples :g !geo:graph)))


;; from Jans ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(<-- (pos->lon/lat ?pos ?lon ?lat)
     (lisp (?lon ?lat)
           (multiple-value-list (upi->longitude-latitude ?pos))))

;; (geo-distance +x +y -dist)

(<-- (geo-distance ?x ?y ?dist)
     (ground ?x)(ground ?y)(not (ground ?dist)) \!
     (q- ?x !geo:isAt5 ?pos1)
     (q- ?y !geo:isAt5 ?pos2)
     (pos->lon/lat ?pos1 ?lon1 ?lat1)
     (pos->lon/lat ?pos2 ?lon2 ?lat2)
     (lisp ?dist (haversine-miles ?lon1 ?lat1 ?lon2 ?lat2)))

(<- (geo-distance ?x ?y ?dist)
    (lisp (error t "Both ?x and ?y should be bound")))

;;  (geo-within-radius +x ±y +miles)

(<-- (geo-within-radius ?x ?y ?miles)
     (ground ?x) (ground ?miles) (not (ground ?y)) \!
     (q- ?x !geo:isAt5 ?pos)
     (pos->lon/lat ?pos ?lon ?lat)
     (triple-inside-haversine-miles ?triple (?? *lat-lon-5*)
                                    !geo:isAt5
                                    ?lon ?lat ?miles)
     (subject ?y ?triple))

(<- (geo-within-radius ?x ?y ?miles)
     (ground ?y) \!
     (geo-distance ?x ?y ?dist)
     (lispp (<= ?dist ?miles)))

(<- (geo-within-radius ?x ?y ?miles)
    (lisp (error "The signature for geo-within-radius is + - +")))


