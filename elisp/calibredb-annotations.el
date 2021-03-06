(require 'dom)
(require 'calibredb)
(require 'calibredb-utils)
(require 'calibredb-core)

(require 'ucs-normalize)

(defun get-annotations(book-id)
  (let* ((json (json-parse-string
              (calibredb-command :command "list"
                                 :id book-id
                                 :option (format "--for-machine --fields '*Annotations' -s id:%s" book-id)
                                 :library (format "--library-path %s" (calibredb-root-dir-quote)))))
         (find-me (aref json 0))
         (find-annotations (gethash "*annotations" find-me))
         (parsed-html (if (not (eq find-annotations nil))
                          (with-temp-buffer
                             (insert find-annotations)
                             (libxml-parse-html-region (point-min) (point-max)))
                        (error "Annotations for book not found")))
         (filtered-annotations (dom-search parsed-html
                                           (lambda (node)
                                             (cond ((and (eq (car node) 'td)
                                                         (dom-by-class node "location"))
                                                    t)
                                                   ((eq (car node) 'p)
                                                    t)
                                                   )))))
    (let ((all-annotations (make-hash-table :test 'equal)))
      (cl-map 'list
              (lambda (node)
                (cond ((eq (car node) 'td)
                       (setq current-book-chapter (dom-text node))
                       )
                      ((eq (car node) 'p)
                       (setq current-book-paragraph (dom-text node))
                       (setq current-book-annotations (gethash current-book-chapter all-annotations '()))
                       (add-to-list 'current-book-annotations current-book-paragraph)
                       (puthash current-book-chapter current-book-annotations all-annotations)
                       )))
              filtered-annotations)
      all-annotations)))

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
