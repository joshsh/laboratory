
;; from http/back/packages.cl
(defpackage :db.agraph.http.backend
 (:nicknames :ags.backend)
 (:use :cl :net.aserve
       :db.agraph :db.agraph.sparql :db.agraph.cursor :st-json :excl
       :ags.shared :ags.formats :ags.error :db.agraph.log)
 (:export #:start-backend #:stop-backend
          #:start-session #:stop-session #:session-alive
          #:supported-reasoners))

(in-package :ags.backend)

;; http/shared/misc.cl
(defmacro scase (value &body clauses)
 (let ((val-name (gensym)))
   `(let ((,val-name ,value))
     (cond ,@(mapcar (lambda (clause)
                       (if (eql (car clause) t)
                           clause
                           (let ((values (if (consp (car clause)) (car clause) (list (car clause))))
                                 (body (cdr clause)))
                             `((or ,@(mapcar (lambda (value) `(equal ,val-name ,value)) values)) ,@body))))
                     clauses)))))

