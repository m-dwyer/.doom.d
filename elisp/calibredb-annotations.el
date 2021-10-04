(require 'dom)
(require 'calibredb)
(require 'calibredb-utils)
(require 'calibredb-core)

(defun get-annotations(book-id)
  (setq json (json-parse-string
              (calibredb-command :command "list"
                                 :id book-id
                                 :option (format "--for-machine --fields '*Annotations' -s id:%s" book-id)
                                 :library (format "--library-path %s" (calibredb-root-dir-quote)))))

  (setq find-me (aref json 0))
  (setq find-annotations (gethash "*annotations" find-me))

  (setq parsed-html
        (with-temp-buffer
          (insert find-annotations)
          (libxml-parse-html-region (point-min) (point-max))))


  (setq filtered-annotations
      (dom-search parsed-html
                  (lambda (node)
                    (cond ((and (eq (car node) 'td)
                                (dom-by-class node "location"))
                           t)
                          ((eq (car node) 'p)
                           t)
                          ))))


  (setq all-annotations (make-hash-table :test 'equal))
  (cl-map 'list
          (lambda (node)
            (cond ((eq (car node) 'td)
                   (setq current-chapter (dom-text node))
                   )
                  ((eq (car node) 'p)
                   (setq current-paragraph (dom-text node))
                   (setq current-annotations (gethash current-chapter all-annotations '()))
                   (add-to-list 'current-annotations current-paragraph)
                   (puthash current-chapter current-annotations all-annotations)
                   )))
          filtered-annotations)
  all-annotations
  )

(defun get-book-id()
  (let* ((book-candidate (calibredb-find-candidate-at-point))
         (book-id (calibredb-getattr (car book-candidate) :id)))
    book-id))

(defun get-annotations-at-point()
  (interactive)
  (setq book-id (get-book-id))

  (let ((books-dir (expand-file-name "books" md--org-resources-dir)))
    (make-directory books-dir :parents))

  (with-temp-buffer
    (maphash
     (lambda (k v)
       (insert (format "* %s" k))
       (insert "\n\n")
       (mapcar (lambda (arg)
                 (insert arg)
                 (insert "\n\n"))
               (nreverse v))
       (insert "\n"))
     (get-annotations book-id))
    (write-region (point-min) (point-max)
     (expand-file-name
      (format "books/%s.org" book-id) md--org-resources-dir))))


(provide 'calibredb-annotations)
