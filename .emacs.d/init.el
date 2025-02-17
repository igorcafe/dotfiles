;; no startup message
(setq inhibit-startup-message t)

;; no scroll bar
(scroll-bar-mode -1)

;; no tool bar
(tool-bar-mode -1)

;; no menu bar
(menu-bar-mode -1)

;; blink cursor
(blink-cursor-mode 1)

;; no bell
(setq ring-bell-function 'ignore)

;; font size
(set-face-attribute 'default nil :height 140)

;; straight.el bootstrap
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

;; use-package
(straight-use-package 'use-package)

;; straight use-package integration
(setq straight-use-package-by-default t)

;; use M-x straight-rebuild-package instead
(setq straight-check-for-modification 'never) 

;; vim simulation
(use-package evil
  :hook (after-init . evil-mode)
  :custom
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  
  :bind
  (("C-x C-h" . previous-buffer)
   ("C-x C-l" . next-buffer)
   ("C-x C-u" . universal-argument)))

;; yousurround for evil
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; evil collection for other modes
(use-package evil-collection
  :after evil)

;; vertical minibuffer
(use-package vertico
  :hook (after-init . vertico-mode)
  :init
  (setq vertico-count 20)
  (setq vertico-cycle t)
  (setq vertico-sort-function 'vertico-sort-history-alpha)
  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)))

;; give more data to help commands
(use-package marginalia
  :hook (after-init . marginalia-mode))

;; better completions
(use-package orderless
  :defer t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; do not annoy me with warning messages
(setq warning-minimum-level :error)

;; do not create annoying files
(setq create-lockfiles nil)
(setq make-backup-files nil)
(auto-save-mode -1)

;; never have to type yes
(defalias 'yes-or-no-p 'y-or-n-p)

;; save custom data separately
(setq custom-file "~/.emacs.d/custom.el")

;; automatically close pairs like ( [ { " '
(electric-pair-mode 1)

;; instead of wrapping, truncate line end
(set-default 'truncate-lines t)

;; tab = indent with 2 spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; automatically revert buffer when the file is changed
(global-auto-revert-mode 1)

;; save a history of commands
(savehist-mode 1)
(setq history-length 200)

;; language support and tools
(dolist (pkg '(go-mode
	             go-tag
	             nix-mode
	             typescript-mode
	             yaml-mode
	             markdown-mode
	             vue-mode))
  (eval `(use-package ,pkg :defer t)))

;; LSP
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (python-mode . lsp-mode))

;; consider this_thing as a whole word
(dolist (hook '(prog-mode-hook
		            text-mode-hook
		            conf-mode-hook))
  (add-hook hook (lambda ()
		               (modify-syntax-entry ?_ "w"))))

(dolist (hook '(text-mode-hook
		            prog-mode-hook
		            conf-mode-hook
		            restclient-mode-hook))
  (add-hook hook 'display-line-numbers-mode))

;; relative line number when in normal mode, absolute in insert mode
(add-hook 'evil-insert-state-entry-hook
          (lambda ()
            (when display-line-numbers
              (setq display-line-numbers-type t)
              (display-line-numbers-mode 1))))

(add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (when display-line-numbers
              (setq display-line-numbers-type 'relative)
              (display-line-numbers-mode 1))))

;; highlight current line
(global-hl-line-mode 1)

;; theme
(use-package doom-themes :defer t)
(load-theme 'doom-one-light t)

;; org mode
(use-package org
  ;; hide drawers by default
  :hook (org-mode . org-fold-hide-drawer-all)
  :custom
  ;; org files
  (org-directory "~/Sync/Org")
  (org-agenda-files '("tasks.org"))
  :bind (("C-'" . org-cycle-agenda-files)))
