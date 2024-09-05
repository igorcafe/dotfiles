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

(setq user-emacs-directory "~/.cache/emacs/")
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

(use-package undo-tree
  :demand t
  :config
  (when (not (file-directory-p "~/.emacs.d/undotree"))
    (make-directory "~/.emacs.d/undotree"))
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undotree")))
  (setq evil-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

(use-package evil
  :demand t
  :custom
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil) ; what? idk
  ;;(evil-want-minibuffer t)
  (evil-undo-system 'undo-tree)
  (evil-cross-lines t)
  :bind
  (:map evil-normal-state-map
        ("SPC u" . universal-argument))
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

;;(add-hook 'visual-line-mode 'adaptive-wrap-prefix-mode)

;; (setq scroll-step 1)
;; (setq scroll-margin 1)
;; (setq scroll-conservatively 1000)
;; (setq scroll-preserve-screen-position 1)

(add-hook 'prog-mode '(setq show-trailing-whitespace t))

(use-package focus :defer)

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
  :config (doom-modeline-mode 1))

(electric-pair-mode 1)

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

(use-package eglot
  :hook
  (before-save . eglot-format)

  :bind
  (:map evil-normal-state-map
        ("gi" . eglot-find-implementation)
        ("SPC l r" . eglot-rename)
        ("SPC l R" . eglot-reconnect)
        ("SPC l a a" . eglot-code-actions)
        ("SPC l a e" . eglot-code-action-extract))
  :init
  (setq eglot-sync-connect nil) ;; do not block when loading lsp


  ;; TODO
  (add-hook 'before-save-hook
            (lambda ()
              (call-interactively 'eglot-code-action-organize-imports))
            t nil))

(use-package eldoc-box
    :config
    (eldoc-box-hover-at-point-mode 1)
    (setq eldoc-echo-area-use-multiline-p 1)
    (advice-add 'eldoc-doc-buffer :override 'eldoc-box-help-at-point))

(use-package corfu
  :config
  (setq corfu-auto nil)
  (setq corfu-preview-current nil)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 1)
  (setq corfu-cycle t)
  (global-set-key (kbd "C-SPC") #'completion-at-point)
  (global-corfu-mode 1))

(use-package cape
  :init
  (dolist (mode '(text-mode-hook
                  prog-mode-hook
                  conf-mode-hook))
    (add-hook mode (lambda ()
                     (add-to-list 'completion-at-point-functions #'cape-tex)
                     (add-to-list 'completion-at-point-functions #'cape-emoji)
                     (add-to-list 'completion-at-point-functions #'cape-file)))))

(use-package dap-mode)



(advice-add 'org-drill-time-to-inactive-org-timestamp :override
            (lambda (time)
              "Convert TIME into org-mode timestamp."
              (format-time-string
               (concat "[" (cdr org-time-stamp-formats) "]")
               time)))

(use-package direnv
  ;; :config
  ;; (direnv-mode)
  )

(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)
(global-set-key "\C-x\ \C-r" 'recentf-open)

(use-package visual-fill-column
  :init
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width 110)
  :hook ((prog-mode eww-mode text-mode conf-mode) . visual-fill-column-mode))

;;(desktop-save-mode 1)

(save-place-mode 1)

(use-package consult
  :defer
  :bind
  (:map evil-normal-state-map
        ("SPC g s" . consult-grep)))

(use-package emacs
  :config
  (setq tab-bar-tab-hints t)
  :bind
  (:map evil-normal-state-map
        ("gc" . tab-bar-close-tab)
        ("gn" . tab-bar-new-tab)
        ("gh" . tab-bar-switch-to-prev-tab)
        ("gl" . tab-bar-switch-to-next-tab)))

(use-package simple-httpd)

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
  :init (global-diff-hl-mode 1))

(use-package blamer :defer)

(use-package perspective
  :bind
  (:map evil-normal-state-map
        ("SPC SPC p" . persp-mode)
        ("SPC SPC s" . persp-switch)
        ("SPC SPC l" . persp-next)
        ("SPC SPC h" . persp-prev)))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

(use-package vterm
  :ensure nil
  :defer
  :init
  (define-key evil-normal-state-map (kbd "SPC t") 'vterm))

(setq global-auto-revert-non-file-buffers t)

(use-package treemacs
  :config
  (setq treemacs-width 40)
  :bind
  (:map global-map
	([f8] . treemacs)))

(use-package emacs
  :bind
  (:map evil-normal-state-map
        (("gb" . evil-switch-to-windows-last-buffer)))
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

(use-package org
  :hook (org-mode . auto-fill-mode))

(defun my/org-fold-hide-drawer-all ()
  (interactive)
  (org-fold-hide-drawer-all))

(use-package org
  :config
  (setq org-directory "~/Git/Org"))

;; (advice-add 'org-refile :after 'org-save-all-org-buffers)

(use-package org
  :config
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60))

(use-package org
  :config
  (setq org-priority-highest 0)
  (setq org-priority-lowest 5)
  (setq org-priority-default 5))

(use-package org
  :bind
  (:map org-mode-map
        ("C-c h" . org-table-move-cell-left)
        ("C-c l" . org-table-move-cell-right)
        ("C-c k" . org-table-move-cell-up)
        ("C-c j" . org-table-move-cell-down)))

(use-package org
  :config
  ;;(setq org-log-done 'item)
  (setq org-hierarchical-todo-statistics nil) ;; TODO recursive by default
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE"))))

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

(setq org-hide-emphasis-markers nil)

(font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org
  :bind
  (:map global-map
        ("C-c c" . org-capture))
  :config
  (setq org-capture-templates
        '(("t"
           "todo item"
           entry
           (file+headline org-default-notes-file "Tasks")
           "* TODO %?\n"))))

(use-package org-roam
  :defer
  :config
  (when (not (file-directory-p "~/Git/Org/Roam"))
    (make-directory "~/Git/Org/Roam"))
  (setq org-roam-directory "~/Git/Org/Roam")

  (org-roam-db-autosync-enable)

  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n d d" . org-roam-dailies-goto-today)
   ("C-c n d y" . org-roam-dailies-goto-yesterday)
   ("C-c n d t" . org-roam-dailies-goto-tomorrow)))

(use-package org-roam-ui :defer)

(use-package org
  :init
  (setq org-deadline-warning-days 3)
  (setq org-scheduled-past-days 0)
  (setq org-agenda-start-day "-5d")
  (setq org-agenda-span 20)
  (setq org-agenda-show-all-dates nil)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-deadline-warning-days 0)
  (setq org-agenda-files
        '("tasks.org"))
  (setq org-agenda-custom-commands
        '(("d" "Today"
           ((agenda "" ((org-agenda-span 'day)
                        (org-agenda-start-day "0d")
                        (org-deadline-warning-days 3)))))))

  ;; default:
  ;; (setq org-agenda-prefix-format
  ;; 		'((agenda . " %i %-12:c%?-12t% s")
  ;; 		 (todo . " %i %-12:c")
  ;; 		 (tags . " %i %-12:c")
  ;; 		 (search . " %i %-12:c")))
  (setq org-agenda-prefix-format
        '((agenda . " %?-12t% s")
          (todo . " ")
          (tags . " ")
          (search . " ")))
  :bind
  (:map global-map
        ("C-c a" . org-agenda)
        ("C-'" . org-cycle-agenda-files)))

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

(use-package org-alert
  :config
  (setq alert-default-style 'libnotify)
  (when (eq system-type 'darwin)
    (setq alert-default-style 'osx-notifier))
  (setq org-alert-interval 60)
  (setq org-alert-notify-cutoff 10)
  (setq org-alert-notify-after-event-cutoff 2)
  (setq org-alert-notification-title "ORG AGENDA")
  (org-alert-enable))

(use-package toc-org
  :hook
  (org-mode . toc-org-mode))

(use-package telega
  :ensure nil ;; installed and built through nix
  :init
  (setq telega-emoji-use-images nil))

(use-package ement :defer)

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
  :config
  (pdf-tools-install))

(use-package emacs
  :hook (eww-mode . visual-line-mode))
