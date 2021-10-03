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


  (setq filtered-anotations
      (dom-search parsed-html
                  (lambda (node)
                    (cond ((and (eq (car node) 'td)
                                (dom-by-class node "location"))
                           t)
                          ((eq (car node) 'p)
                           t)
                          ))))


  (setq all-annotations (make-hash-table))
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
          filtered-anotations)

  (setq hash-out '())
  (with-temp-buffer
    (maphash
     (lambda (k v)
       (insert k)
       (insert "\n")
       (mapcar (lambda (arg) (insert arg)) v)
       (insert "\n\n")
       )
     all-annotations)
    (write-region (point-min) (point-max) "/home/zara/test.org")
    )

  all-annotations
  )

(get-annotations)

