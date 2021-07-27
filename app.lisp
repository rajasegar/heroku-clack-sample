(ql:quickload '(:djula :mito :flexi-streams :quri :cl-ppcre))

(djula:add-template-directory  #P"templates/")
(defparameter *template-registry* (make-hash-table :test 'equal))

;; render template - copied & modified from caveman
(defun render (template-path &optional data)
  (let ((html (make-string-output-stream))
	(template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template* template html data)
    `(200 (:content-type "text/html")
	  (,(format nil "~a" (get-output-stream-string html))))))

;; utils
;; (defun get-requestp (url env)
;;   (and (string= url (getf env :path-info)) (equal (getf env :request-method) :get)))

(defun get-requestp (url env)
  (and (cl-ppcre:scan-to-strings (concatenate 'string "^"  url "$") (getf env :path-info)) (equal (getf env :request-method) :get)))

(defun post-requestp (url env)
  (and (string= url (getf env :path-info)) (equal (getf env :request-method) :post)))

(defun put-requestp (url env)
  (and (string= url (getf env :path-info)) (equal (getf env :request-method) :put)))

(defun get-parsed (env)
  "Get parsed form-encoded values from :raw-body"
  (let ((body (make-array (getf env :content-length) :element-type 'flexi-streams:octet)))
    (read-sequence body (getf env :raw-body))
    (quri:url-decode-params (flexi-streams:octets-to-string body :external-format :utf-8))))

(defun get-param (name parsed)
  "Get parameter values from parsed"
  (cdr (assoc name parsed :test #'string=)))

(defun get-query-param (name querystring)
  "Get query param values from :query-string"
  (cdr (assoc name (quri:url-decode-params querystring) :test #'string=)))

(defun get-id-from-url (regex url)
  (cl-ppcre:register-groups-bind (id) (regex url)
    (parse-integer id)))

;; db
(mito:connect-toplevel :sqlite3 :database-name #P"movies.db")

;; mito tables
(mito:deftable movie ()
  ((title :col-type (:varchar 50))
   (rating :col-type :integer)))

(mito:deftable theatre ()
  ((name :col-type (:varchar 30))
   (movie :col-type movie)))

(mito:ensure-table-exists 'movie)
(mito:ensure-table-exists 'theatre)

;; (mito:insert-dao (make-instance 'movie :title "Lord of the Rings" :rating 9))
;; (mito:insert-dao (make-instance 'movie :title "The Matrix" :rating 8))
;; (mito:insert-dao (make-instance 'movie :title "2012" :rating 8))
;; (mito:insert-dao (make-instance 'movie :title "Independence Day" :rating 8))
;; (mito:insert-dao (make-instance 'movie :title "Titanic" :rating 8))

(lambda (env)
  (print env)
  (cond
    ;; about page
    ((get-requestp "/about" env)
     (render #P"about.html"))
    ;; contact page
    ((get-requestp "/contact" env)
     (render #P"contact.html"))
    ;; movies
    ((get-requestp "/movies" env)
     (render #P"movies.html" (list :movies (mito:select-dao 'movie))))

    ;; POST movies
    ((post-requestp "/movies" env)
     (let* ((parsed (get-parsed env))
	    (title (get-param "title" parsed))
	    (rating (get-param "rating" parsed)))
       (print title)
       (mito:insert-dao (make-instance 'movie :title title :rating rating))
       (render #P"movies.html" (list :movies (mito:select-dao 'movie)))))

    ;; new movies
    ((get-requestp "/movies/new" env)
     (render #P"new-movies.html"))
    ;; theatres
    ((get-requestp "/theatres" env)
     (render #P"theatres.html" (list :theatres (mito:select-dao 'theatre (mito:includes 'movie)))))

    ;; new theatre
    ((get-requestp "/theatres/new" env)
    ;; ((get-requestp "/theatres/new" env)
     (render #P"new-theatre.html" (list :movies (mito:select-dao 'movie))))

    ;; create theatre
    ((post-requestp "/theatres" env)
     (let* ((parsed (get-parsed env))
	    (name (get-param "name" parsed))
	    (movie (get-param "movie" parsed)))
       (print parsed)
       (mito:insert-dao (make-instance 'theatre :name name :movie (mito:find-dao 'movie :id (parse-integer movie))))
       (render #P"theatres.html" (list :theatres (mito:select-dao 'theatre)))))

    ;; edit theatre
    ((get-requestp "/theatres/([0-9]+)/edit" env)
     ;; (print (getf env :query-string))
     (let ((id (get-id-from-url "^/theatres/([0-9]+)/edit$" (getf env :path-info))))
       (render #P"edit-theatre.html" (list :theatre (mito:find-dao 'theatre :id id)
					 :movies (mito:select-dao 'movie)))))

    ;; update theatre
    ((put-requestp "/theatres" env)
     (let* ((parsed (get-parsed env))
	    (name (get-param "name" parsed))
	    (movie (get-param "movie" parsed)))
       (print parsed)
       (mito:insert-dao (make-instance 'theatre :name name :movie (mito:find-dao 'movie :id (parse-integer movie))))

       (render #P"theatres.html" (list :theatres (mito:select-dao 'theatre)))))
    
    ;; home
    ((get-requestp "/" env)
     (render #P"index.html"))
    ;; default route
    (t
     (render #P"404.html"))))
