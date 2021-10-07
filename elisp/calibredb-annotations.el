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

  (if (eq find-annotations nil)
      (error "Annotations for book not found"))

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

(defun get-book-metadata()
  (let* ((book-candidate (calibredb-find-candidate-at-point))
         (id (calibredb-getattr (car book-candidate) :id))
         (title (calibredb-getattr (car book-candidate) :book-title))
         (author (calibredb-getattr (car book-candidate) :author-sort)))
    (progn `((id . ,id) (title . ,title) (author . ,author)))))

(defun get-book-annotations()
  (let* ((book-metadata (get-book-metadata))
         (book-id (cdr (assoc 'id book-metadata)))
         (annotations (get-annotations book-id)))
    (cons `(annotations . ,annotations) book-metadata)))

(provide 'calibredb-annotations)
