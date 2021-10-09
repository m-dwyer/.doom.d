;;; elisp/utils.el -*- lexical-binding: t; -*-
(require 'calibredb-annotations)

(defun md/annotations-to-org-file ()
  (let* ((book-details (get-book-annotations))
         (book-title (cdr (assoc 'title book-details)))
         (book-annotations (cdr (assoc 'annotations book-details)))
         (book-author (cdr (assoc 'author book-details))))

  (with-temp-buffer
    (maphash
     (lambda (k v)
       (insert (format "* %s" k))
       (insert "\n\n")
       (mapc (lambda (arg)
                 (insert arg)
                 (insert "\n\n"))
               (nreverse v))
       (insert "\n"))
     book-annotations)
    (buffer-string))))


(defun md/org-roam-capture-book-annotations ()
  (interactive)

  (let ((books-dir (expand-file-name "books" md--org-resources-dir)))
    (make-directory books-dir :parents))

  (let* ((book-metadata (get-book-metadata))
        (book-title (cdr (assoc 'title book-metadata)))
        (book-author (cdr (assoc 'author book-metadata))))

    (org-roam-capture- :node (org-roam-node-create :title (progn book-title))
                     :templates `(("b" "book note" plain (function md/annotations-to-org-file)
                                  :target
                                   (file+head
                                    ,(concat
                                      "books/"
                                      (format-time-string "%Y%m%d%H%M%S")
                                      "-${slug}.org")
                                    ,(concat
                                     "#+title: ${title}\n"
                                     "#+author: %(plist-get org-capture-plist :author)\n"
                                     "#+filetags: books"))
                                   :immediate-finish t
                                   :jump-to-captured t
                                   :empty-lines 1
                                   :author ,book-author)))))
