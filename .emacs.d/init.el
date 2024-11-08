(toggle-frame-fullscreen)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(eval-when-compile (require 'use-package))
(setq use-package-always-ensure t)

(use-package org-auto-tangle
  :defer
  :hook (org-mode . org-auto-tangle-mode))

(defalias 'yes-or-no-p 'y-or-n-p)

;;(setq warning-minimum-level :emergency)

(setq create-lockfiles nil)

(setq user-emacs-directory "~/.emacs.d/")
(when (not (file-directory-p user-emacs-directory))
  (make-directory user-emacs-directory))

(setq make-backup-files nil)

(setq auto-save-file-name-transforms
  `((".*" "~/.cache/emacs/" t)))

(setq custom-file "~/.emacs.d/custom.el")

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package esup
  :defer
  :config
  (setq esup-depth 0))

(use-package evil
  :demand t
  :custom
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil) ; what? idk
  ;;(evil-want-minibuffer t)
  (evil-undo-system 'undo-redo)
  (evil-cross-lines t)
  :bind
  (:map evil-normal-state-map
        ("SPC u" . universal-argument)
        ("H" . previous-buffer)
        ("L" . next-buffer))
  :config
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package key-chord
  :after evil
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(use-package emacs
  :hook ((prog-mode
          text-mode
          conf-mode)
         . (lambda ()
             (modify-syntax-entry ?_ "w"))))

(column-number-mode 1) ;; TODO

(use-package emacs
      :hook ((evil-insert-state-entry
		      . (lambda ()
			      (setq display-line-numbers-type t)
			      (display-line-numbers-mode 1)))
		 (evil-insert-state-exit
		      . (lambda ()
			      (setq display-line-numbers-type 'relative)
			      (display-line-numbers-mode 1)))))

(use-package emacs
  :hook ((text-mode
          prog-mode
          conf-mode) . display-line-numbers-mode))

;;(global-visual-line-mode 1)

(set-default 'truncate-lines t)

(setq-default fill-column 80)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;;(add-hook 'visual-line-mode 'adaptive-wrap-prefix-mode)

;; (setq scroll-step 1)
;; (setq scroll-margin 1)
;; (setq scroll-conservatively 1000)
;; (setq scroll-preserve-screen-position 1)

(add-hook 'prog-mode '(setq show-trailing-whitespace t))

(use-package focus :defer)

(use-package emacs
  :config
  (global-hl-line-mode 1))

(use-package evil-mc :defer)

(use-package doom-themes
  :defer 0.3
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-one t))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; run once
;;(all-the-icons-install-fonts t)
;;(nerd-icons-install-fonts t)

(use-package doom-modeline
  :defer 1
  :config
  (setq doom-modeline-buffer-name nil)
  (setq doom-modeline-buffer-encoding nil)
  (doom-modeline-mode 1))

(use-package breadcrumb
  :hook
  ((prog-mode
    conf-mode
    text-mode
    vterm-mode)
   . breadcrumb-local-mode))

(fringe-mode 8)

(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'line)
  (setq sideline-backends-right '(sideline-flymake)))

(electric-pair-mode 1)

(defun project-vterm ()
  (interactive)
  (let* ((proj-dir (car (last (project-current))))
         (proj-name (file-name-nondirectory
                     (directory-file-name "~/Git/backend/")))
         (chosen-name (read-string "buffer name: " proj-name))
         (default-directory proj-dir))
    (vterm (format "vterm - %s" chosen-name))))

(use-package project
  :preface
  :config
  (setq project-switch-commands
        '((project-find-file "Find file" ?f)
          (project-find-regexp "Find regexp" ?g)
          (project-find-dir "Find directory" ?d)
          (project-vterm "vterm" ?t)
          ;;(project-vc-dir "VC-Dir")
          ;;(project-eshell "Eshell")
          ;;(project-any-command "Other")
          (magit-project-status "Magit" ?m))))

(use-package go-mode
  :defer
  :hook
  (go-mode . eglot-ensure))

(use-package go-tag :defer)

(use-package nix-mode :defer)

(use-package yaml-mode :defer)

(use-package markdown-mode :defer)

(use-package emacs
  :hook (python-mode . eglot-ensure))

(use-package emacs
  :config
  (setq nxml-child-indent 4)
  (setq nxml-attribute-indent 4))

(use-package vue-mode)

(use-package emacs
  :hook (c-mode . (lambda ()
                    (setq c-basic-offset 2)
                    (setq indent-tabs-mode nil))))

(use-package eglot
  :hook
  (before-save
   . (lambda ()
       (when (bound-and-true-p eglot-managed-p)
         (call-interactively 'eglot-format)
         (call-interactively 'eglot-code-action-organize-imports))))

  :bind
  (:map evil-normal-state-map
        ("gi" . eglot-find-implementation)
        ("SPC l r" . eglot-rename)
        ("SPC l R" . eglot-reconnect)
        ("SPC l a a" . eglot-code-actions)
        ("SPC l a e" . eglot-code-action-extract))
  :init
  ;; do not block when loading lsp
  (setq eglot-sync-connect nil))

(use-package eldoc-box
    :config
    (eldoc-box-hover-at-point-mode 1)
    (setq eldoc-echo-area-use-multiline-p 1)
    (advice-add 'eldoc-doc-buffer :override 'eldoc-box-help-at-point))

(use-package corfu
  :hook ((text-mode prog-mode conf-mode) . corfu-mode)
  :config
  (setq corfu-auto nil)
  (setq corfu-preview-current nil)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 1)
  (setq corfu-cycle t)
  :bind
  (:map global-map
        ("C-SPC" . completion-at-point)))

(use-package cape
  :init
  (dolist (mode '(text-mode-hook
                  prog-mode-hook
                  conf-mode-hook))
    (add-hook mode (lambda ()
                     (add-to-list 'completion-at-point-functions #'cape-tex)
                     (add-to-list 'completion-at-point-functions #'cape-emoji)
                     (add-to-list 'completion-at-point-functions #'cape-file)))))

(use-package envrc
  :config
  (envrc-global-mode))

(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)
(global-set-key "\C-x\ \C-r" 'recentf-open)

(use-package olivetti
  :hook ((prog-mode
          eww-mode
          text-mode
          conf-mode
          org-agenda-mode)
         . olivetti-mode)
  :init
  (setq-default olivetti-body-width 100))

;;(desktop-save-mode 1)

(save-place-mode 1)

(use-package consult
  :defer
  :bind
  (:map evil-normal-state-map
        ("SPC g s" . consult-git-grep)
        ("SPC l e" . consult-flymake)))

(use-package emacs
  :config
  (setq tab-line-switch-cycling t)
  :bind
  (:map evil-normal-state-map
        ("SPC k" . kill-this-buffer)
        ("SPC SPC l" . tab-line-switch-to-next-tab)
        ("SPC SPC h" . tab-line-switch-to-prev-tab)))

(use-package emacs
  :config
  ;; (setq tab-bar-tab-hints t)
  (setq tab-bar-show nil)
  :bind
  (:map evil-normal-state-map
        ("gc" . tab-bar-close-tab)
        ("gn" . tab-bar-new-tab)
        ("gh" . tab-bar-switch-to-prev-tab)
        ("gl" . tab-bar-switch-to-next-tab)))

(use-package whitespace
  :hook
  ((prog-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face tabs spaces trailing space-mark tab-mark)))

(use-package restclient :defer t)

(use-package simple-httpd :defer t)

(use-package yasnippet
  :config
  (yas-define-snippets
   'go-mode
   '(("iferr" "if err != nil {\n\treturn err${1:}\n}")
     ("iferr2" "if err != nil {\n\treturn nil${1:}, err${2:}\n}")
     ("lv" "log.Printf(\"%#v\", ${1:})")))

  (yas-define-snippets
   'org-mode
   '(("#el" "#+begin_src emacs-lisp\n${1:}\n#+end_src\n")
     ("#mus" "** ${1:}\n:PROPERTIES:\n:TYPE: song\n:END:\n")))

  (yas-global-mode 1)
  :bind
  (:map yas-minor-mode-map
        ("<tab>" . yas-expand)
        ("C-SPC" . yas-next-field-or-maybe-expand)))

(use-package emacs
  :bind
  (:map evil-normal-state-map
        ("C-w u" . winner-undo)
        ("C-w C-r" . winner-redo))
  :config
  (winner-mode 1))

(use-package magit :defer)

(use-package diff-hl
  :defer 1
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :init
  (global-diff-hl-mode 1))

(use-package blamer :defer)

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(defun my/vterm (name)
  (interactive "sname: ")
  (vterm (concat "vterm - " name)))

(use-package vterm
  :ensure nil
  :defer
  :bind
  (:map evil-normal-state-map
        (("SPC t" . my/vterm))))

(use-package emacs
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (setq global-auto-revert-non-file-buffers t)
  :bind
  (:map dired-mode-map
        ("<tab>" . dired-find-file-other-window)))

(use-package emacs
  :bind
  (:map evil-normal-state-map
        (("gb" . evil-switch-to-windows-last-buffer)
         ("M-p" . evil-prev-buffer)
         ("M-n" . evil-next-buffer)))
  :config
  (global-auto-revert-mode 1))

(use-package emacs
  :bind ("C-x C-b" . ibuffer))

(savehist-mode 1)
(setq history-length 100)

(use-package vertico
  :config
  (vertico-mode)
  (vertico-mouse-mode)
  (setq vertico-count 20)
  (setq vertico-cycle t)

  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)))

(use-package vertico-posframe
  :config (vertico-posframe-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(setq org-src-window-setup 'current-window)

(setq org-indirect-buffer-display 'current-window)

(use-package org
  :hook (org-mode . auto-fill-mode))

(use-package org
  :config
  (setq org-sparse-tree-default-date-type 'active))

(use-package org
  :config
  (setq org-directory "~/Sync/Org")
  (setq org-outline-path-complete-in-steps t)
  (setq org-refile-targets nil)
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(use-package org
  :config
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60))

(use-package org
  :config
  (setq org-priority-highest ?A)
  (setq org-priority-lowest ?D)
  (setq org-priority-default ?D)
  (setq org-priority-faces
        ;; nil
        '((?A . (:foreground "gray"))
          (?B . (:foreground "gray"))
          (?C . (:foreground "gray"))
          (?D . (:foreground "gray")))
        ))

(use-package org
  :config
  (setq org-tags-column -90)
  (setq org-tag-alist nil))

(use-package org
  :bind
  (:map org-mode-map
        ("C-c h" . org-table-move-cell-left)
        ("C-c l" . org-table-move-cell-right)
        ("C-c k" . org-table-move-cell-up)
        ("C-c j" . org-table-move-cell-down)))

(use-package org
  :config
  (setq org-log-into-drawer t)
  (setq org-log-done nil)
  (setq org-log-reschedule t)
  (setq org-log-redeadline t)
  (setq org-hierarchical-todo-statistics t) ;; TODO cookie count not recursive
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n!)" "WAIT(w@)" "|" "DONE(d!)" "CANC(c@)")
          (sequence "PROJ(p)" "|" "FINI(f!)")))
  :bind
  (("C-c C-x C-o" . org-clock-out)
   ("C-c C-x C-j" . org-clock-goto)))

(defun my/clocktable-write (&rest args)
  (apply #'org-clocktable-write-default args)
  (save-excursion
    (forward-char)
    (org-table-move-column-right)
    (org-table-move-column-right)))

(setq org-duration-format 'h:mm)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org
  :hook (org-mode . org-indent-mode))

(defvar my/org-big-fonts '((org-document-title . 1.8)
                           (org-level-1 . 1.6)
                           (org-level-2 . 1.4)
                           (org-level-3 . 1.2)))
(defun my/org-big ()
  (interactive)
  (dolist (face my/org-big-fonts)
    (set-face-attribute (car face) nil :height (cdr face))))

(defun my/org-smol ()
  (interactive)
  (dolist (face my/org-big-fonts)
    (set-face-attribute (car face) nil :height 1.0)))

(setq org-hide-emphasis-markers t)

(use-package org-appear
    :hook
    (org-mode . org-appear-mode))

(font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-fragtog
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-startup-with-latex-preview t)
  :custom
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

(use-package org
  :bind
  (:map global-map
        ("C-c c" . org-capture))
  :config
  (setq org-capture-templates
        '(("c"
           "Capture to inbox"
           entry
           (file+headline "tasks.org" "Tasks")
           "* TODO %?\n%U")
          ("j" "Journal"
           entry
           (file+headline "journal.org" "Journal")
           "* %T - %?"))))

(use-package org-roam
  :defer
  :config
  (when (not (file-directory-p "~/Sync/Org/Roam"))
    (make-directory "~/Sync/Org/Roam"))
  (setq org-roam-directory "~/Sync/Org/Roam")

  (org-roam-db-autosync-enable)

  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)))

(use-package org-roam-ui :defer)

(defun my/org-sort ()
  (interactive)
  (org-sort-entries nil ?T)
  (org-sort-entries nil ?p)
  (org-sort-entries nil ?o))

(defun my/org-agenda-show-all-dates ()
  (interactive)
  (setq org-agenda-show-all-dates
        (if org-agenda-show-all-dates nil t))
  (org-agenda-redo))

(defun my/org-agenda-breadcrumb ()
  (let ((parent (cdr (org-get-outline-path))))
    (if parent
        (format "[%s] " (mapconcat 'identity parent " > "))
      "")))


(use-package org
  :init
  (setq org-scheduled-past-days 100
        org-agenda-start-with-log-mode nil
        org-agenda-window-setup 'current-window
        org-agenda-block-separator ?―
        org-agenda-start-day nil
        org-agenda-tags-column 'auto
        org-agenda-span 1
        org-agenda-show-all-dates nil
        org-agenda-skip-deadline-if-done t
        org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2)
        org-agenda-skip-scheduled-if-done nil
        org-deadline-warning-days 3
        org-agenda-start-with-follow-mode nil
        org-agenda-compact-blocks nil
        org-agenda-use-time-grid t
        org-agenda-skip-archived-trees nil
        org-agenda-current-time-string "←"
        org-agenda-files '("tasks.org")
        org-agenda-log-mode-items '(closed state)
        org-stuck-projects '("TODO=\"PROJ\"" ("NEXT" "WAIT") nil "")

        org-agenda-todo-keyword-format "%s"
        org-agenda-prefix-format '((agenda . "  %-12t %s %(my/org-agenda-breadcrumb)")
                                   (todo . "  %(my/org-agenda-breadcrumb)")
                                   (tags . "  %(my/org-agenda-breadcrumb)")
                                   (search . "  %(my/org-agenda-breadcrumb)"))

        org-agenda-time-grid
        '((daily today require-timed)
          (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200)
          " ┄┄┄┄┄ " "")

        org-agenda-custom-commands
        '(("p" "Projects"
           ((todo "PROJ"
                      ((org-agenda-overriding-header "Projects")))
           ))
          ("a" "Agenda"
           ((agenda ""
                    ((org-agenda-span 10)
                     (org-scheduled-past-days 100)
                     (org-deadline-warning-days 10)))))
          ("d" "To-do"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-agenda-time-grid '((daily today require-timed)
                        ()
                        " ┄┄┄┄┄ " ""))))
            (tags-todo "+PRIORITY=\"A\""
                       ((org-agenda-overriding-header "Urgent")))
            (tags-todo "-TODO=\"WAIT\"-PRIORITY=\"C\""
                       ((org-agenda-overriding-header "In progress")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'notregexp "CLOCK: \\[." 'scheduled))))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting")
                   (org-agenda-sorting-strategy '(alpha-up))))
            (todo "NEXT"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-sorting-strategy '(alpha-up))
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if
                           'regexp "CLOCK: \\[."
                           'scheduled))))
            (tags-todo "+TODO=\"TODO\"+PRIORITY=\"D\"+LEVEL=2"
                       ((org-agenda-overriding-header "Stuff")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if
                           'regexp "CLOCK: \\[."
                           'scheduled 'done))))
            (tags-todo "+PRIORITY=\"C\"+LEVEL=2"
                       ((org-agenda-overriding-header "Later")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled 'done))))))
          ("w" "Agenda"
           ((agenda ""
                    ((org-agenda-files '("work.org"))
                     (org-agenda-span 100)
                     (org-scheduled-past-days 0)
                     (org-deadline-warning-days 0)))))
          ("e" "Tasks by effort"
           ((tags-todo "-TODO=\"DONE\"-TODO=\"FINI\"+Effort>\"\""
                       ((org-agenda-overriding-header "Tasks by effort")
                        (org-agenda-sorting-strategy '(effort-up))
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled))
                        (org-agenda-prefix-format '((tags . "%-5e - ")))))))
          ("E" "Tasks without effort"
           ((tags-todo "+TODO=\"TODO\"+Effort=\"\""
                       ((org-agenda-overriding-header "Tasks without effort")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled))))))))

  (custom-set-faces
   '(org-agenda-current-time ((t (:foreground "green" :weight bold)))))

  :bind
  ((:map global-map
         ("C-c a" . org-agenda)
         ("C-'" . org-cycle-agenda-files))
   (:map org-agenda-mode-map
         ("C-a" . my/org-agenda-show-all-dates)
         ("j" . org-agenda-next-line)
         ("C-j" . org-agenda-goto-date)
         ("h" . org-agenda-earlier)
         ("l" . org-agenda-later)
         ("C-d" . evil-scroll-down)
         ("C-w C-w" . evil-window-next)
         ("C-u" . evil-scroll-up)
         ("M-g" . org-agenda-toggle-time-grid)
         ("{" . org-agenda-backward-block)
         ("}" . org-agenda-forward-block)
         ("z" . evil-scroll-line-to-center)
         ("g" . evil-goto-first-line)
         ("G" . evil-goto-line)
         ("k" . org-agenda-previous-line))))

(use-package org-present
  :defer
  :hook ((org-present-mode
          . (lambda ()
              (org-present-hide-cursor)
			      (setq display-line-numbers-type nil)
              (display-line-numbers-mode 1)))
         (org-present-mode-quit
          . (lambda ()
              (org-present-show-cursor)
			      (setq display-line-numbers-type 'relative)
              (display-line-numbers-mode 1)))))

(use-package org-drill
  :config
  (add-to-list 'org-modules 'org-drill))

(advice-add 'org-drill-time-to-inactive-org-timestamp :override
            (lambda (time)
              "Convert TIME into org-mode timestamp."
              (format-time-string
               (concat "[" (cdr org-time-stamp-formats) "]")
               time)))

(use-package emacs
  :after notifications
  :config
  (setq appt-message-warning-time 60
        appt-display-interval 10
        appt-display-mode-line nil)

  (setq appt-disp-window-function
        (lambda (remaining new-time msg)
          (notifications-notify
           :title (format "In %s minutes" remaining)
           :body msg
           :urgency 'critical)))

  (advice-add 'appt-check :before
              (lambda (&rest args)
                (org-agenda-to-appt t)))
  (appt-activate t))

(use-package toc-org
  :hook
  (org-mode . toc-org-mode))

(use-package telega
  :ensure nil ;; installed and built through nix
  :init
  (setq telega-emoji-use-images nil))

(use-package elfeed
  :defer
  :config
  (setq elfeed-feeds
        '(
          ;; DHH
          "https://world.hey.com/dhh/feed.atom" 
          ;; Martin Fowler
          "https://martinfowler.com/feed.atom" 
          ;; Go Blog
          "https://go.dev/blog/feed.atom" 
          ;; ThePrimeTime
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCUyeluBRhGPCW4rPe_UvBZQ" 
          ;; Mental Outlaw
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" 
          ;; Fireship
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCsBjURrPoezykLs9EqgamOA" 
          ;; Lakka News
          "https://www.lakka.tv/articles/feed.xml" 
          )))

(use-package pdf-tools
  :defer
  :config
  (pdf-tools-install))

(use-package emacs
  :hook (eww-mode . visual-line-mode)
  :config
  (setq eww-retrieve-command
        ;;'("google-chrome-stable" "--headless" "--dump-dom")
        nil
        ))

(use-package emacs
  :config
  (setq display-time-day-and-date t)
  (setq display-time-format "%a %H:%M %d/%m")
  (setq display-time-default-load-average nil)
  (display-time-mode 1)
  (display-battery-mode 1))

(defun advice-remove-all (sym)
  "Remove all advices from symbol SYM."
  (interactive "aFunction symbol:")
  (advice-mapc `(lambda (fun props)
                  (advice-remove ,(quote sym) fun))
               sym))

(use-package emms
  :config
  (emms-all)
  (emms-default-players)
  :bind
  (:map evil-normal-state-map
        ("SPC m j" . emms-next)
        ("SPC m k" . emms-previous)
        ("SPC m h" . emms-seek-backward)
        ("SPC m l" . emms-seek-forward)
        ("SPC m SPC" . emms-pause)
        ("SPC m s" . emms-stop)
        ("SPC m e" . emms)))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(defun my/org-music-play-song-at-point ()
  (interactive)
  (org-music-play-song-at-point))


(require 'org-music) ;; idk why it only works that way

(use-package org-music
  :load-path "lisp/org-music.el"
  :after emms
  :init
  (setq
   org-music-file nil
   org-music-youtube-downloader "yt-dlp"
   org-music-media-directory "~/.cache/org-music"
   org-music-operating-system "linux"
   org-music-cache-size (* 10 1024)) ;; 10 GB?

  :bind
  (:map evil-normal-state-map
        ("SPC m p l" . org-music-play-list)
        ("SPC m p p" . my/org-music-play-song-at-point)))
