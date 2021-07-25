(ql:quickload '(:djula :mito :flexi-streams :quri))

(djula:add-template-directory  #P"templates/")

(defparameter +index.html+ (djula:compile-template* "index.html"))

(defparameter +about.html+ (djula:compile-template* "about.html"))

(defparameter +contact.html+ (djula:compile-template* "contact.html"))

(defparameter +movies.html+ (djula:compile-template* "movies.html"))
(defparameter +new-movies.html+ (djula:compile-template* "new-movies.html"))
(defparameter +theatres.html+ (djula:compile-template* "theatres.html"))
(defparameter +404.html+ (djula:compile-template* "404.html"))

;; render template
(defun render (template &optional data)
  (let ((html (make-string-output-stream)))
    (apply #'djula:render-template* template html data)
    `(200 (:content-type "text/html")
          (,(format nil "~a" (get-output-stream-string html))))))

;; utils
(defun is-get (url env)
  (and (string= url (getf env :request-uri)) (equal (getf env :request-method) :get)))

(defun is-post (url env)
  (and (string= url (getf env :request-uri)) (equal (getf env :request-method) :post)))

(defun get-param (name body)
  (cdr (assoc name body :test #'string=)))

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
  ;; (print env)
  (cond
    ;; about page
    ((string= "/about" (getf env :request-uri))
     (render +about.html+))
    ;; contact page
    ((string= "/contact" (getf env :request-uri))
     (render +contact.html+))
    ;; movies
    ((is-get "/movies" env)
     (render +movies.html+ (list :movies (mito:select-dao 'movie))))
    ;; POST movies
    ((is-post "/movies" env)
     (let ((body (make-array (getf env :content-length) :element-type 'flexi-streams:octet)))
       (read-sequence body (getf env :raw-body))
       (print (flexi-streams:octets-to-string body :external-format :utf-8))
       (let* ((parsed (quri:url-decode-params (flexi-streams:octets-to-string body :external-format :utf-8)))
              (title (get-param "title" parsed))
              (rating (get-param "rating" parsed)))

         (mito:insert-dao (make-instance 'movie :title title :rating rating))

         (render +movies.html+ (list :movies (mito:select-dao 'movie))))))

    ;; new movies
    ((string= "/movies/new" (getf env :request-uri))
     (render +new-movies.html+))
    ;; movies
    ((string= "/theatres" (getf env :request-uri))
     (render +theatres.html+))
    ;; home
    ((string= "/" (getf env :request-uri))
     (render +index.html+))
    ;; default route
    (t
     (render +404.html+))))
