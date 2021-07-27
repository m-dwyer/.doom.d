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
(setq doom-font (font-spec :family "Fira Mono" :size 32)
      doom-variable-pitch-font (font-spec :family "Cantarell" :size 32))

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

  (setq md--org-templates-dir (expand-file-name "templates" user-emacs-directory))
  (setq md--org-journal-dir (expand-file-name "journal" org-directory))
  (setq md--org-reviews-dir (expand-file-name "reviews" org-directory))
  (setq md--org-projects-dir (expand-file-name "projects" org-directory))

  (setq md--org-project-template (expand-file-name "project.org" md--org-templates-dir))
  (setq md--org-weekly-review-template (expand-file-name "weekly-review.org" md--org-templates-dir))

  (setq md--org-tasks (expand-file-name "tasks.org" org-directory))
  (setq md--org-recurring-tasks (expand-file-name "recurring.org" org-directory))
  (setq md--org-incubate (expand-file-name "incubate.org" org-directory))

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files `(,md--org-tasks ,md--org-recurring-tasks ,md--org-projects-dir))

  (setq org-enforce-todo-dependencies t)

  (setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "DOING(s)" "WAIT(w)" "|" "DONE(d!)" "CANCELLED(c)")
        (sequence "RECURRING" "|" "DONE")
        ))

  (setq org-tags-exclude-from-inheritance '("project"))

  ;; Show actionable tasks
  (setq org-agenda-custom-commands
        '(("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))
          ("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))
          ("e" "Low Effort" tags-todo "+TODO=\"NEXT\"+Effort<=15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)))
          ("W" "Work Tasks" tags-todo "+@work"))))
