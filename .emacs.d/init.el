(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(use-package org-auto-tangle
  :defer
  :hook (org-mode . org-auto-tangle-mode))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq warning-minimum-level :emergency)

(setq create-lockfiles nil)

(setq user-emacs-directory "~/.cache/emacs/")
(when (not (file-directory-p user-emacs-directory))
  (make-directory user-emacs-directory))

;; wtf
(setq backup-directory-alist
      `((".*" . ,(expand-file-name "backups" user-emacs-directory)))
				   backup-by-copying t
				   version-control t
				   delete-old-versions t
				   vc-make-backup-files t
				   kept-old-versions 10
				   kept-new-versions 10)

(setq custom-file "~/Git/dotfiles/.emacs.d/custom.el")

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package esup
  :defer
  :config
  (setq esup-depth 0))

(set-face-attribute 'default nil :height 140)
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :height 160))

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
  (evil-want-C-u-scroll t) ; C-u won't be enabled by default
  (evil-want-keybinding nil) ; what? idk
  (evil-want-minibuffer t)
  (evil-undo-system 'undo-tree)
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

(dolist (mode '(text-mode-hook
               prog-mode-hook
               conf-mode-hook))
  (add-hook mode #'display-line-numbers-mode))

;;(global-visual-line-mode 1)

(set-default 'truncate-lines t)

(setq-default tab-width 4)

;;(add-hook 'visual-line-mode 'adaptive-wrap-prefix-mode)

;; (setq scroll-step 1)
;; (setq scroll-margin 1)
;; (setq scroll-conservatively 1000)
;; (setq scroll-preserve-screen-position 1)

(add-hook 'prog-mode '(setq show-trailing-whitespace t))

(use-package focus :defer)

(use-package evil-mc :defer)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(blink-cursor-mode 0)
(setq ring-bell-function 'ignore) ; this is actually sound, but...

(toggle-frame-fullscreen)

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
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 1)
  (setq corfu-cycle t)
  (global-set-key (kbd "C-SPC") #'completion-at-point)
  (global-corfu-mode 1))

(use-package dap-mode)

(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)
(global-set-key "\C-x\ \C-r" 'recentf-open)

(use-package visual-fill-column
  :init
  (setq visual-fill-column-center-text t)
  (setq visual-fill-column-width 110)
  :config
  (global-visual-fill-column-mode 1))

(desktop-save-mode 1)

(save-place-mode 1)

(use-package consult
  :defer
  :bind
  (:map evil-normal-state-map
        ("SPC g s" . consult-grep)))

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

(savehist-mode 1)
(setq history-length 100)

(use-package vertico
  :config
  (vertico-mode 1)
  (setq vertico-count 20)
  (setq vertico-cycle t)
  (keymap-set vertico-map "C-j" #'vertico-next)
  (keymap-set vertico-map "C-k" #'vertico-previous))

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
  :config
  (setq org-directory "~/Org")
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

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
  (setq org-log-done 'item)
  (setq org-hierarchical-todo-statistics nil) ;; TODO recursive by default
  (setq org-todo-keywords
        '((sequence "TODO" "|" "DONE"))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org
  :hook (org-mode . org-indent-mode))

(use-package org
      :hook
      (org-mode
       . (lambda ()
	       (dolist (face '((org-document-title . 1.8)
					       (org-level-1 . 1.6)
					       (org-level-2 . 1.4)
					       (org-level-3 . 1.2)))
		 (set-face-attribute (car face) nil :height (cdr face))))))

(setq org-hide-emphasis-markers t)

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

(use-package org
      :init
      (setq org-agenda-files
		'("Roam/20240620102058-tasks.org"))
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
		("C-c a" . org-agenda)))

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

(use-package org-alert
  :config
  (setq org-alert-interval 300)
  (setq org-alert-notify-cutoff 10)
  (setq org-alert-notify-after-event-cutoff 10))

(use-package org-roam
  :defer
  :config
  (when (not (file-directory-p "~/Org/Roam"))
    (make-directory "~/Org/Roam"))
  (setq org-roam-directory "~/Org/Roam")

  (org-roam-db-autosync-enable)

  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n d d" . org-roam-dailies-goto-today)))

(use-package org-roam-ui :defer)

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
          "https://world.hey.com/dhh/feed.atom" ; DHH
          "https://martinfowler.com/feed.atom" ; Martin Fowler
          "https://go.dev/blog/feed.atom" ; Go Blog
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCUyeluBRhGPCW4rPe_UvBZQ" ; ThePrimeTime
          "https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" ; Mental Outlaw
          "https://www.youtube.com/feeds/videos.xml?channel_id=UCsBjURrPoezykLs9EqgamOA" ; Fireship
          "https://www.lakka.tv/articles/feed.xml" ; Lakka News
          "https://thehackernews.com/feeds/posts/default" ; The Hacker News
          )))

(use-package pdf-tools
  :config
  (pdf-tools-install))
