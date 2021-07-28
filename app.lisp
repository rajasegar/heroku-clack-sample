(ql:quickload '(:djula :mito :flexi-streams :quri :cl-ppcre :sxql))

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
(defun get-requestp (url env)
  (and (cl-ppcre:scan-to-strings (concatenate 'string "^"  url "$") (getf env :path-info)) (equal (getf env :request-method) :get)))

(defun post-requestp (url env)
  (and (cl-ppcre:scan-to-strings url (getf env :path-info)) (equal (getf env :request-method) :post)))

(defun put-requestp (url env)
  (and (cl-ppcre:scan-to-strings url (getf env :path-info)) (equal (getf env :request-method) :put)))

(defun delete-requestp (url env)
  (and (cl-ppcre:scan-to-strings url (getf env :path-info)) (equal (getf env :request-method) :delete)))

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
    ((post-requestp "^/movies$" env)
     (let* ((parsed (get-parsed env))
	    (title (get-param "title" parsed))
	    (rating (get-param "rating" parsed))
	    (new-movie (make-instance 'movie :title title :rating rating)))
       (print title)
       (mito:insert-dao new-movie)
       (render #P"_movie-row.html" (list :movie new-movie :bg "alert-success"))))

    ;; new movies
    ((get-requestp "/movies/new" env)
     (render #P"new-movies.html"))

    ;; edit movie
    ((get-requestp "^/movies/([0-9]+)/edit$" env)
     (let* ((id (get-id-from-url "^/movies/([0-9]+)/edit$" (getf env :path-info)))
	    (movie (mito:find-dao 'movie :id id)))
       (render #P"_edit-movie.html" (list :movie movie))))

    ;; update movie
    ((post-requestp "^/movies/([0-9]+)/update$" env)
     (let* ((id (get-id-from-url "^/movies/([0-9]+)/update$" (getf env :path-info)))
	    (movie (mito:find-dao 'movie :id id))
	    (parsed (get-parsed env))
	    (title (get-param "title" parsed))
	    (rating (get-param "rating" parsed)))
       (setf (slot-value movie 'title) title
	     (slot-value movie 'rating) rating)
       (mito:save-dao movie)
       (render #P"_movie-row.html" (list :movie movie :bg "alert-info"))))

    ;; GET /movies/delete
    ((get-requestp "^/movies/([0-9]+)/delete$" env)
     (let* ((id (get-id-from-url "^/movies/([0-9]+)/delete$" (getf env :path-info)))
	    (movie (mito:find-dao 'movie :id id)))
       (render #P"_confirm-movie-delete.html" (list :movie movie))))
    
    ;; DELETE /movies/:id
    ((delete-requestp "^/movies/([0-9]+)$" env)
     (let* ((id (get-id-from-url "^/movies/([0-9]+)$" (getf env :path-info))))
       (mito:delete-by-values 'movie :id id)
       (render #P"_movie-list.html" (list :movies (mito:select-dao 'movie)))))

    ;; POST /movies/search
    ((post-requestp "^/movies/search$" env)
     (let* ((parsed (get-parsed env))
	    (query (get-param "query" parsed))
	    (movies (mito:select-dao 'movie (sxql:where (:like :title (concatenate 'string "%" query "%"))))))
       (render #P"_movie-list.html" (list :movies movies))))

    ;; theatres
    ((get-requestp "/theatres" env)
     (render #P"theatres.html" (list :theatres (mito:select-dao 'theatre (mito:includes 'movie)))))

    ;; new theatre
    ((get-requestp "/theatres/new" env)
    ;; ((get-requestp "/theatres/new" env)
     (render #P"new-theatre.html" (list :movies (mito:select-dao 'movie))))

;; create theatre
    ((post-requestp "^/theatres$" env)
     (let* ((parsed (get-parsed env))
	    (name (get-param "name" parsed))
	    (movie (get-param "movie" parsed)))
       (print parsed)
       (mito:insert-dao (make-instance 'theatre :name name :movie (mito:find-dao 'movie :id (parse-integer movie))))
       '(302 (:location "/theatres") nil)))

        ;; edit theatre
    ((get-requestp "/theatres/([0-9]+)/edit" env)
     ;; (print (getf env :query-string))
     (let ((id (get-id-from-url "^/theatres/([0-9]+)/edit$" (getf env :path-info))))
       (render #P"edit-theatre.html" (list :theatre (mito:find-dao 'theatre :id id)
					 :movies (mito:select-dao 'movie)))))

    ;; update theatre
    ((post-requestp "^/theatres/([0-9]+)/update$" env)
     (let* ((id (get-id-from-url "^/theatres/([0-9]+)/update$" (getf env :path-info)))
	    (theatre (mito:find-dao 'theatre :id id))
	    (parsed (get-parsed env))
	    (name (get-param "name" parsed))
	    (movie (get-param "movie" parsed)))
       (setf (slot-value theatre 'name) name
	     (slot-value theatre 'movie) (mito:find-dao 'movie :id movie))
       (mito:save-dao theatre)
       '(302 (:location "/theatres") nil)
     ))

    ;; bulk-delete movies
    ((post-requestp "^/movies/bulk-delete$" env)
     (let ((parsed (get-parsed env)))
       (print parsed)
       (dolist (item parsed)
	 (mito:delete-by-values 'movie :id (cdr item)))
       (render #P"_movie-list.html" (list :movies (mito:select-dao 'movie)))))

    ;; home
    ((get-requestp "/" env)
     (render #P"index.html"))
    ;; default route
    (t
     (render #P"404.html"))))
