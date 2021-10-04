(require 'dom)
(require 'calibredb)

(defun get-annotations()
  (setq json (json-parse-string
              (calibredb-command :command "list"
                                 :id "57"
                                 :option "--for-machine --fields '*Annotations' -s id:57"
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

  (with-temp-buffer
    (maphash
     (lambda (k v)
       (insert k)
       (insert "\n\n")
       (mapcar (lambda (arg)
                 (insert arg)
                 (insert "\n\n"))
               (nreverse v))
       (insert "\n"))
     all-annotations)
    (write-region (point-min) (point-max) "/Users/em.dwyer/another.org"))
  )

(get-annotations)
