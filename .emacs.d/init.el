(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(setq straight-check-for-modification '(find-when-checking))

(use-package org-auto-tangle
  :hook (org-mode . org-auto-tangle-mode))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq warning-minimum-level :error)

(setq create-lockfiles nil)

(setq user-emacs-directory "~/.emacs.d/")

(setq make-backup-files nil)

(setq auto-save-file-name-transforms
  `((".*" "~/.cache/emacs/" t)))

(setq custom-file "~/.emacs.d/custom.el")

(use-package esup
  :config
  (setq esup-depth 0))

(use-package emacs
  :hook ((prog-mode
          text-mode
          conf-mode)
         . (lambda ()
             (modify-syntax-entry ?_ "w"))))

(column-number-mode 1)

(use-package emacs
  :hook ((text-mode
          prog-mode
          conf-mode) . display-line-numbers-mode))

(use-package emacs
  :after evil
  :hook ((evil-insert-state-entry
          . (lambda ()
              (when display-line-numbers
                (setq display-line-numbers-type t)
                (display-line-numbers-mode 1))))
         (evil-insert-state-exit
          . (lambda ()
              (when display-line-numbers
                (setq display-line-numbers-type 'relative)
                (display-line-numbers-mode 1))))))

(set-default 'truncate-lines t)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(use-package emacs
  :config
  (global-auto-revert-mode 1))

(use-package emacs
  :bind ("C-x C-b" . ibuffer))

(savehist-mode 1)
(setq history-length 100)

(use-package emacs
  :config
  (setq display-time-day-and-date t)
  (setq display-time-format "%a %H:%M ")
  (setq display-time-default-load-average nil)
  (display-time-mode 1)
  (display-battery-mode 1))

(use-package emacs
  :preface
  (defun my/rename-minibuffer()
    (let* ((orig-buffer
            (window-buffer (minibuffer-selected-window)))
           (new-minibuf-name
            (format "*Minibuf-1* - %s" (buffer-name orig-buffer))))
      (rename-buffer new-minibuf-name)))
  :hook (minibuffer-setup . my/rename-minibuffer))

(use-package evil
  :demand t
  :straight t
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
  :straight t
  :config
  (evil-collection-init))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package key-chord
  :after evil
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(use-package emacs
  :config
  (global-hl-line-mode 1))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; run once
;;(all-the-icons-install-fonts t)
;;(nerd-icons-install-fonts t)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-one t))

(use-package doom-modeline
  :config
  (setq doom-modeline-buffer-name nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-version nil)
  (setq doom-modeline-lsp nil)
  (setq doom-modeline-lsp-icon nil)
  (setq doom-modeline-env-enable-python nil)
  (doom-modeline-mode 1))

(use-package breadcrumb
  :hook
  ((prog-mode
    conf-mode
    text-mode
    vterm-mode)
   . breadcrumb-local-mode))

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
                     (directory-file-name proj-dir)))
         (chosen-name (read-string "buffer name: " proj-name))
         (default-directory proj-dir))
    (vterm (format "vterm - %s" chosen-name))))

(use-package project
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
  :hook
  (go-mode . eglot-ensure))

(use-package go-tag)

(use-package nix-mode)

(use-package yaml-mode)

(use-package markdown-mode)

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
  ;; before saving, if eglot is enabled, try to format and organize imports
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

(use-package emacs
  :config
  (setq eldoc-echo-area-use-multiline-p 1))

(use-package eldoc-box
    :config
    (eldoc-box-hover-at-point-mode 1)
    (advice-add 'eldoc-doc-buffer :override 'eldoc-box-help-at-point))

(use-package corfu
  :hook ((text-mode prog-mode conf-mode) . corfu-mode)
  :config
  (setq corfu-auto nil)
  (setq corfu-preview-current nil)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 1)
  (setq corfu-cycle t)
  (corfu-popupinfo-mode 1)
  :bind
  (:map global-map
        ("C-SPC" . completion-at-point)))

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

(save-place-mode 1)

(use-package consult
  :bind
  (:map evil-normal-state-map
        ;; analogous to project-find-regexp
        ("SPC p g" . consult-git-grep)

        ;; analogous to project-find-file
        ("SPC p f" . consult-project-buffer)

        ;; buffer errors
        ("SPC b e" . consult-flymake)

        ;; buffer definitions
        ("SPC b d" . consult-imenu)))

(use-package emacs
  :config
  (setq tab-line-switch-cycling t))

(use-package emacs
  :config
  (setq tab-bar-show nil))

(use-package whitespace
  :hook
  ((prog-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face tabs spaces trailing space-mark tab-mark)))

(use-package restclient)

(use-package simple-httpd)

(use-package emacs
  :bind
  (:map evil-normal-state-map
        ("C-w u" . winner-undo)
        ("C-w C-r" . winner-redo))
  :config
  (winner-mode 1))

(use-package dired
  :straight nil
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (setq global-auto-revert-non-file-buffers t)
  ;; :bind
  ;; (:map dired-mode-map
  ;;       ("S-TAB" . dired-find-file-other-window))
  )

(use-package dired-subtree
    :bind
    (:map dired-mode-map
          ("TAB" . dired-subtree-toggle)))

(use-package magit
  :bind
  ("C-x g" . magit))

(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (after-save . diff-hl-update))
  :config
  (global-diff-hl-mode 1))

(use-package blamer)

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(defun my/vterm (name)
  (interactive "sname: ")
  (vterm (concat "vterm - " name)))

(use-package vterm
  :straight nil
  :bind
  (:map evil-normal-state-map
        (("SPC t" . my/vterm))))

(use-package vertico
  :init
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (setq vertico-count 20)
  (setq vertico-cycle t)
  (setq vertico-sort-function 'vertico-sort-history-alpha)

  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package org
  :hook (org-mode . auto-fill-mode))

(use-package emms
  :config
  (emms-all)
  (emms-default-players)

  ;; all my songs are downloaded from youtube with org-music and don't
  ;; have metadata.
  ;; so emms always show the full file path in the modeline, which is always
  ;; "path/to/songs/Author - Song Name.m4a"
  ;; this function replaces it by only "🎵 Song Name"
  (setq emms-mode-line-mode-line-function
        (lambda ()
          (let* ((path (emms-track-description
                        (emms-playlist-current-selected-track)))
                 (song (when (string-match ".* - \\(.*\\)\\.m4a$" path)
                         (match-string 1 path))))
            (format "🎵 %s  " song))))
  :bind
  (:map evil-normal-state-map
        ("SPC m j" . emms-next)
        ("SPC m k" . emms-previous)
        ("SPC m ," . emms-seek-backward)
        ("SPC m ." . emms-seek-forward)
        ("SPC m SPC" . emms-pause)
        ("SPC m s" . emms-stop)
        ("SPC m e" . emms)))

(use-package telega
  :straight nil ;; installed and built through nix
  :init
  (setq telega-use-images t)
  (setq telega-emoji-use-images nil)
  (setq telega-sticker-size '(8 . 48))
  (setq telega-chat-group-messages-for nil) ;; (not (or saved-messages (type channel bot)))
  (setq telega-emoji-font-family "Noto Color Emoji"))

(use-package elfeed
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
  :hook (pdf-view-mode . pdf-view-themed-minor-mode)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package nov)

(use-package emacs
  :hook (eww-mode . visual-line-mode)
  :config
  ;; name buffers as [ domain ] - [ title ]
  (setq eww-auto-rename-buffer
        (lambda ()
          (let ((domain
                 (url-host
                         (url-generic-parse-url (plist-get eww-data :url))))
                (title (plist-get eww-data :title)))
            (format "%s - %s # eww"
                    (truncate-string-to-width domain 20 nil nil "...")
                    (truncate-string-to-width title 30 nil nil "..."))))))

(use-package activity-watch-mode
  :config
  (global-activity-watch-mode 1))

(use-package gptel
  :config
  (setq gptel-api-key nil))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq
   initial-buffer-choice (lambda ()
                           (get-buffer-create dashboard-buffer-name))
   dashboard-startup-banner 'logo
   dashboard-center-content t
   dashboard-vertically-center-content t
   dashboard-banner-logo-title nil
   dashboard-items '((recents   . 5)
                     (projects  . 3)
                     (agenda    . 5))))

(setq org-directory "~/Sync/Org")

(setq org-src-window-setup 'current-window)

(setq org-indirect-buffer-display 'current-window)

(use-package org
  :config
  (setq org-outline-path-complete-in-steps t)
  (setq org-refile-targets nil)
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

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
  (setq org-tags-column -89))

(use-package org
  :config
  (setq org-log-into-drawer t)
  (setq org-log-done nil)
  (setq org-log-reschedule t)
  (setq org-log-redeadline t)
  (setq org-hierarchical-todo-statistics t) ;; TODO cookie count not recursive
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE")))
  :bind
  ;; the keybindings are the same, just made them global
  (("C-c C-x C-o" . org-clock-out)
   ("C-c C-x C-j" . org-clock-goto)))

(use-package org
  :bind
  (("C-c C-x C-o" . org-clock-out)
   ("C-c C-x C-j" . org-clock-goto)))

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
           "* INBX %?\n%U")
          ("j" "Journal"
           entry
           (file+headline "journal.org" "Journal")
           "* %T - %?"))))

(font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

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


(use-package org-agenda
  :straight nil
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
        org-agenda-scheduled-leaders '(" " "!")
        org-agenda-deadline-leaders '(" " "!")

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
            (todo "NEXT"
                       ((org-agenda-overriding-header "In progress")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'notregexp "CLOCK: \\[." 'scheduled))))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting")
                   (org-agenda-sorting-strategy '(alpha-up))))
            (tags-todo "+TODO=\"NEXT\"+LEVEL=2"
                       ((org-agenda-overriding-header "Single step tasks")
                        (org-agenda-sorting-strategy '(alpha-up))
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if
                           'regexp "CLOCK: \\[."
                           'scheduled))))
            (todo "PROJ"
                  ((org-agenda-overriding-header "Projects")))
            (todo "INBX"
                       ((org-agenda-overriding-header "Inbox")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if
                           'regexp "CLOCK: \\[."
                           'scheduled 'done))))
            (todo "SMDY"
                       ((org-agenda-overriding-header "Someday")
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
           ((tags-todo "+Effort=\"\""
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

(use-package org-present
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

(use-package toc-org
  :hook
  (org-mode . toc-org-mode))

(defun org-music-jump-to-current-song ()
  (interactive)
  (find-file org-music-file)
  (let* ((song-path (emms-track-name
                     (emms-playlist-current-selected-track)))
         (outline-name (when (string-match ".*/\\(.*\\)\\.m4a" song-path)
                         (match-string 1 song-path)))

         (outline-marker (org-find-exact-headline-in-buffer outline-name)))

    (when outline-marker
      (goto-char outline-marker))))

(use-package org-music
  :straight
  (:host github :repo "debanjum/org-music" :branch "master")
  :after emms
  :init
  (setq
   org-music-file "~/Sync/Org/music.org"
   org-music-youtube-downloader "yt-dlp"
   org-music-media-directory "~/.cache/org-music"
   org-music-operating-system "linux"
   org-music-cache-size (* 10 1024)) ;; 10 GB?
  :bind
  (:map evil-normal-state-map
        ("SPC m c" . org-music-jump-to-current-song)
        ("SPC m l p" . org-music-play-list)
        ("SPC m l e" . org-music-enqueue-list)
        ("SPC m p p" . org-music-play-song-at-point)
        ("SPC m p e" . org-music-enqueue-song-at-point)))

(use-package org
  :hook (org-mode . org-indent-mode))

(use-package org-appear
    :hook
    (org-mode . org-appear-mode))

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

(use-package org-roam
  :config
  (when (not (file-directory-p "~/Sync/Org/Roam"))
    (make-directory "~/Sync/Org/Roam"))
  (setq org-roam-directory "~/Sync/Org/Roam")

  (org-roam-db-autosync-enable)

  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)))

(use-package org-roam-ui)
