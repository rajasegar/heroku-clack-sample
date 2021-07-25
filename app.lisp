(ql:quickload '(:djula))

(djula:add-template-directory  #P"templates/")

(defparameter +index.html+ (djula:compile-template* "index.html"))

(defparameter +about.html+ (djula:compile-template* "about.html"))

(defparameter +contact.html+ (djula:compile-template* "contact.html"))

(defun render (template)
  (let ((html (make-string-output-stream)))
    (djula:render-template* template html)
    `(200 (:content-type "text/html")
          (,(format nil "~a" (get-output-stream-string html))))))


(lambda (env)
  (cond
    ;; about page
    ((string= "/about" (getf env :request-uri))
     (render +about.html+))
    ;; contact page
    ((string= "/contact" (getf env :request-uri))
     (render +contact.html+))
    ;; default route
    (t
     (render +index.html+))))
