;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "m dwyer"
      user-mail-address "mdwyer@mdwyer.io")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "JetBrains Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "Cantarell" :size 16)
      doom-big-font (font-spec :family "JetBrains Mono" :size 22))

(setq org-hide-emphasis-markers t)

(add-hook! 'org-mode-hook (lambda ()  (org-superstar-mode 1)))

(use-package! mixed-pitch
  :config
  (add-hook 'org-mode-hook #'mixed-pitch-mode))

(use-package! org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t)
  (setq org-appear-autosubmarkers t)
  (setq org-appear-autolinks t)
)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(use-package! beacon
  :config
  (beacon-mode 1))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; General config
;;
;; Reload buffer when file changes on disk
(global-auto-revert-mode t)

(use-package! org
  :config
  (setq org-ellipsis " ▾")

  (setq md--org-templates-dir (expand-file-name "templates" doom-private-dir))
  (setq md--org-journal-dir (expand-file-name "journal" org-directory))
  (setq md--org-projects-dir (expand-file-name "projects" org-directory))
  (setq md--org-archive-dir (expand-file-name "archive" org-directory))
  (setq md--org-plan-dir (expand-file-name "plan" org-directory))
  (setq md--org-areas-dir (expand-file-name "areas" org-directory))
  (setq md--org-yearly-template (expand-file-name "yearly.org" md--org-templates-dir))
  (setq md--org-monthly-template (expand-file-name "monthly.org" md--org-templates-dir))
  (setq md--org-weekly-template (expand-file-name "weekly.org" md--org-templates-dir))
  (setq md--org-daily-template (expand-file-name "daily.org" md--org-templates-dir))
  (setq md--org-project-template (expand-file-name "project.org" md--org-templates-dir))
  (setq md--org-area-template (expand-file-name "area.org" md--org-templates-dir))

  (setq md--org-goals (expand-file-name "goals.org" org-directory))
  (setq md--org-tasks (expand-file-name "tasks.org" org-directory))
  (setq md--org-recurring-tasks (expand-file-name "recurring.org" org-directory))
  (setq md--org-incubate (expand-file-name "incubate.org" org-directory))

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files `(,md--org-goals ,md--org-tasks ,md--org-recurring-tasks ,md--org-projects-dir))

  (setq org-enforce-todo-dependencies t)

  (setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "DOING(s)" "WAIT(w)" "|" "DONE(d!)" "CANCELLED(c)")
        (sequence "RECURRING" "|" "RECURDONE")
        (sequence "GOAL" "|" "ACHIEVED")
        ))

  (setq org-tags-exclude-from-inheritance '("project"))
)

(defun md/org-mode-visual()
  (setq visual-fill-column-width 100
       visual-fill-column-center-text t
       display-fill-column-indicator nil
       display-line-numbers nil)
  (visual-fill-column-mode 1))

(add-hook! 'org-mode-hook
           #'md/org-mode-visual #'+org-pretty-mode)

(after! org
  (custom-set-faces!
    '(org-document-title :height 1.5)
    '(org-level-1 :inherit outline-1 :weight extra-bold :height 1.4)
    '(org-level-2 :inherit outline-2 :weight bold :height 1.15)
    '(org-level-3 :inherit outline-3 :weight bold :height 1.12)
    '(org-level-4 :inherit outline-4 :weight bold :height 1.09)
    '(org-level-5 :inherit outline-5 :weight semi-bold :height 1.06)
    '(org-level-6 :inherit outline-6 :weight semi-bold :height 1.03)
    '(org-level-7 :inherit outline-7 :weight semi-bold)
    '(org-level-8 :inherit outline-8 :weight semi-bold)
    ;; Ensure that anything that should be fixed-pitch in org buffers appears that
    ;; way
    '(org-block nil :foreground nil :inherit 'fixed-pitch)
    '(org-code nil   :inherit '(shadow fixed-pitch))
    '(org-table nil   :inherit '(shadow fixed-pitch))
    '(org-verbatim nil :inherit '(shadow fixed-pitch))
    '(org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    '(org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    '(org-checkbox nil :inherit 'fixed-pitch)))

(setq org-tag-alist '((:startgrouptag)
                      ("Goal")
                      (:grouptags)
                      ("{G@.+}")
                      (:endgrouptag)
                      (:startgrouptag)
                      ("Area")
                      (:grouptags)
                      ("{A@.+}")
                      (:endgrouptag)))

(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq org-agenda-custom-commands
        '(("t" "Today view"
           ((agenda "" ((org-agenda-overriding-header "")
                        (org-agenda-span 'day)
                        (org-agenda-start-day nil)
                        ;; always show timelines!
                        (org-agenda-time-grid '((daily today) (800 1000 1200 1400 1600 1800 2000) "......" "----------------"))
                        (org-super-agenda-groups
                         '((:name "Scheduled Today"
                            :time-grid t
                            :date today
                            :order 1)
                           (:name "Habits"
                            :habit t
                            :date today
                            :order 2)
                           (:name "Overdue"
                            :deadline past
                            :order 3)
                           (:name "Ongoing"
                            :scheduled past
                            :order 4
                            )
                           (:discard (:anything t)))
                         )
                        )
                    )
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t%-6e% s")
                                                     (todo . " %i %-12:c %-6e")
                                                     (tags . " %i %-12:c")
                                                     (search . " %i %-12:c")))
                         (org-super-agenda-groups
                          '((:name "Low Effort (<= 15 min)"
                             :and (:effort< "0:16")
                             :order 1)
                            (:name "Next Tasks"
                             :todo "NEXT"
                             :order 2)
                            (:discard (:anything t))))))))
          ("w" "Week view"
           ((agenda "" ((org-agenda-overriding-header "Week view")
                        (org-agenda-span 'week)
                        (org-agenda-start-on-weekday 1)
                        )
                )
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Overdue (past scheduled/deadline)"
                             :deadline past
                             :scheduled past
                             :order 1
                             )
                            (:name "Individual Tasks"
                             :file-path "task"
                             :order 2
                             )
                            (:name "Next tasks"
                             :todo "NEXT"
                             :order 3)
                            (:discard (:anything t))
                            )
                          )
                         )
            )
            )
           )
          ("p" . "Planning")
          ("pm" "Month view"
           (
            (tags-todo "+Goal" ((org-agenda-overriding-header "Goals")
                        )
                    )
            (agenda "" ((org-agenda-span 'month)
                        (org-agenda-start-day "01")
                        (org-super-agenda-groups
                         '((:discard (:todo "GOAL"))
                           (:discard (:todo "RECURRING"))
                           (:scheduled t))
                         )
                        )
                    )
            (todo "" ((org-agenda-overriding-header "Things to schedule")
                        (org-super-agenda-groups
                         '((:name "Individual tasks"
                            :file-path "task"
                           )
                           (:name "Next tasks"
                            :todo "NEXT"
                            )
                           (:discard (:anything t)))
                         )
                        )
                    )
           ))
          ))
  :config
  (org-super-agenda-mode))

(defun md/prompt-date (prompt variable)
  (set variable (org-read-date nil 'to-time nil prompt)))

(defun md/prompt-string (prompt variable)
  (set variable (read-string prompt))
  )

(defun md/get-project-filename (name)
  (expand-file-name
   (format "%s.org" (s-dashed-words name)) md--org-projects-dir))

(defun md/get-area-filename (name)
  (expand-file-name
   (format "%s.org" (s-dashed-words name)) md--org-areas-dir))

(defun md/get-planning-filename (&optional period plandate)
  (or plandate (setq plandate (current-time)))
  (or period (setq period 'week))
  (format "%s.org" (pcase period
                           ('day (format-time-string "%Y-%m-%d" plandate))
                           ('week (format-time-string "%Y-%m-%B-W%V" plandate))
                           ('month (format-time-string "%Y-%m-%B" plandate))
                           ('year (format-time-string "%Y" plandate))
                           )
                )
  )

(defun md/get-planning-file (&optional period plandate)
  (expand-file-name (md/get-planning-filename period plandate) md--org-plan-dir)
  )

(setq org-capture-templates
      `(("p" "Planning")
        ("pa" "Area" plain
         (file (lambda () (md/get-area-filename (md/prompt-string "Area Name:" 'md--org-capture-area))))
         (file ,md--org-area-template))
        ("pp" "Project" entry
         (file (lambda () (md/get-project-filename (md/prompt-string "Project Name:" 'md--org-capture-project))))
         (file ,md--org-project-template))
        ("py" "Yearly Plan" plain
         (file (lambda() (md/get-planning-file 'year (md/prompt-date "Year:" 'md--org-capture-planning-year))))
         (file ,md--org-yearly-template))
        ("pm" "Monthly Plan" plain
         (file (lambda() (md/get-planning-file 'month (md/prompt-date "Month:" 'md--org-capture-planning-month))))
         (file ,md--org-monthly-template))
        ("pw" "Weekly Plan" plain
         (file (lambda () (md/get-planning-file 'week (md/prompt-date "Week:" 'md--org-capture-planning-week))))
         (file ,md--org-weekly-template))
        ("pd" "Daily Plan" entry
         (file+olp (lambda () (md/get-planning-file 'week (md/prompt-date "Day:" 'md--org-capture-planning-day)))
          "Weekly Planning" "Dailies")
         (file ,md--org-daily-template))
        ("t" "Task" entry (file+headline md--org-tasks "Tasks")
         "* TODO %?\n %U\n %a\n %i" :empty-lines 1)))

(use-package! org-edna
  :hook
  '(org-mode . org-edna-mode)
  :config
  (setq org-edna-use-inheritance t))

(setq org-archive-location
      (concat (file-name-as-directory
               (expand-file-name (format-time-string "%Y" (current-time))
                           md--org-archive-dir)
         )
        "%s_archive::datetree/")
)

(use-package! org-journal
  :after org
  :config
  (setq org-journal-dir md--org-journal-dir)
  (setq org-journal-file-type 'weekly)
  (setq org-journal-date-format "%A, %d %B %Y")
  (setq org-journal-file-format (md/get-planning-filename 'week))
  (setq org-journal-file-header "#+TITLE: Weekly Journal W%V\n#+STARTUP: folded"))
