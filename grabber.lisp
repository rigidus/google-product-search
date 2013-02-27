(require 'cl-fad)
(require 'split-sequence)
(require 'alexandria)
(require 'cl-ppcre)
(setf ppcre:*use-bmh-matchers* nil)
(require 'drakma)
(require 'hunchentoot)
(require 'cl-json)


(defparameter *key* "AIzaSyDXNN-OfxFCBbo6owLD7SsJ7UiU9nxGLE8")
(defparameter *user-agent* "Mozilla/5.0 (X11; U; Linux i686; ru; rv:1.9.2.13) Gecko/20101206 Ubuntu/10.04 (lucid) Firefox/3.6.13")

;; "https://www.googleapis.com/shopping/search/v1/public/products?key=key&country=US&q=%22mp3+player%22&restrictBy=brand%3Asony"

(defun process-page (uri-str)
  (let ((addr (format nil uri-str *key*)))
    (multiple-value-bind (body-or-stream status-code headers uri stream-out must-close reason-phrase)
        (drakma:http-request addr)
      (if (equal 200 status-code)
          (json:decode-json-from-string (sb-ext:octets-to-string body-or-stream))
          (error (format nil "~A ~A : ~A~%" status-code reason-phrase uri)
          )))))


(defun get-products (min-cnt start-link)
  (let ((link start-link)
        (products (make-hash-table :test #'equal)))
    (do ((i 0 (1+ i)))
        ((> (hash-table-count products) min-cnt) products)
      (let* ((data  (process-page link))
             (items (cdr (assoc :items data))))
        (setf link (cdr (assoc :next-link data)))
        (loop :for item :in items :collect
           (let ((product (cdr (assoc :product item))))
             (print product)
             (setf (gethash (cdr (assoc :name (cdr (assoc :author product)))) products)
                   (cdr (assoc :link product)))))))))

(maphash #'(lambda (k v)
             (format t "~%~A~%  ~A" k v))
         (get-products 2 "https://www.googleapis.com/shopping/search/v1/public/products?key=~A&country=US&q=home+accessories&alt=json"))


(loop :for num :from 1 :to 999 :do
   (maphash #'(lambda (k v)
                (format t "~%~A~%  ~A" k v))
            (get-products 2 "https://www.googleapis.com/shopping/search/v1/public/products?key=~A&country=US&q=home+accessories&alt=json")))
