(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(setq use-package-always-ensure t)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(blink-cursor-mode 0)
(setq ring-bell-function 'ignore) ; this is actually sound, but...

(toggle-frame-fullscreen)

(set-face-attribute 'default nil :height 140)

(use-package doom-themes
  :defer 0.3
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-one t))

(use-package all-the-icons
  :if (display-graphic-p))

;; run once
;;(all-the-icons-install-fonts t)
;;(nerd-icons-install-fonts t)

(use-package doom-modeline
  :defer 1
  :config (doom-modeline-mode 1))

(add-hook 'prog-mode '(setq show-trailing-whitespace t))

(setq warning-minimum-level :emergency)

(column-number-mode 1) ;; TODO

(setq display-line-numbers-type 'relative)

(dolist (mode '(text-mode-hook
               prog-mode-hook
               conf-mode-hook))
  (add-hook mode #'display-line-numbers-mode))

(global-visual-line-mode 1)

(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 1000)
(setq scroll-preserve-screen-position 1)

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

(savehist-mode 1)
(setq history-length 100)

(use-package vertico
  :config
  (vertico-mode 1)
  (keymap-set vertico-map "C-j" #'vertico-next)
  (keymap-set vertico-map "C-k" #'vertico-previous))

(use-package vertico-posframe
  :config (vertico-posframe-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.2)
  (setq corfu-auto-prefix 1)
  (setq corfu-cycle t)
  (global-set-key (kbd "C-SPC") #'completion-at-point)
  (global-corfu-mode 1))

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-secondary-delay 0.1))

(desktop-save-mode 1)

(save-place-mode 1)

(recentf-mode 1)
(setq recentf-max-menu-items 100)
(setq recentf-max-saved-items 100)
(global-set-key "\C-x\ \C-r" 'recentf-open)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t) ; for dired

(electric-pair-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package pdf-tools
  :defer
  :config
  (pdf-tools-install))

(use-package org-bullets :defer)

(add-hook 'org-mode-hook (lambda()
                             (org-bullets-mode 1)
                             (org-indent-mode 1)
                             (set-face-attribute 'org-document-title nil :height 1.8)
                             (set-face-attribute 'org-level-1 nil :height 1.8)
                             (set-face-attribute 'org-level-2 nil :height 1.5)
                             (set-face-attribute 'org-level-3 nil :height 1.2)
                             (org-overview)))

(setq org-hide-emphasis-markers t)

;; org mode lists
;; (font-lock-add-keywords 'org-mode
;;     '(("^ *\\([-]\\) "
;;     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-roam
  :defer
  :config
  (when (not (file-directory-p "~/.Roam"))
    (make-directory "~/.Roam"))
  (setq org-roam-directory "~/.Roam")

  (org-roam-db-autosync-enable)

  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)))

(use-package org-roam-ui :defer)

(use-package eglot
  :hook
  (before-save . eglot-format)

  :init
  ;; do not block when loading lsp
  (setq eglot-sync-connect nil)

  ;; don't use more than one line for eldoc, unless called with K
  (setq eldoc-echo-area-use-multiline-p 1)

  (define-key evil-normal-state-map (kbd "gi") 'eglot-find-implementation))

(use-package eldoc-box
  :config
  (eldoc-box-hover-at-point-mode 1))

(use-package go-mode
  :defer
  :hook
  (go-mode . eglot-ensure))

(use-package nix-mode :defer)

(use-package yaml-mode :defer)

(use-package evil
  :demand t
  :init
  (setq evil-want-C-u-scroll t) ; C-u won't work by default
  (setq evil-want-keybinding nil) ; what? idk
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-normal-state-map (kbd "gb") 'evil-switch-to-windows-last-buffer)
  (define-key evil-normal-state-map (kbd "TT") 'tab-bar-switch-to-tab)
  (define-key evil-normal-state-map (kbd "Th") 'tab-previous)
  (define-key evil-normal-state-map (kbd "Tl") 'tab-next)
  (define-key evil-normal-state-map (kbd "Tn") 'tab-new)
  (advice-add 'evil-scroll-up :after 'evil-scroll-line-to-center)
  (advice-add 'evil-scroll-down :after 'evil-scroll-line-to-center)
  (define-key evil-normal-state-map (kbd "Tc") 'tab-close))

(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

(use-package key-chord
  :after evil
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

(use-package magit :defer)

(use-package diff-hl
  :defer 1
  :init (global-diff-hl-mode 1))

(use-package blamer
  :defer 1
  :config
  (global-blamer-mode 1))

(use-package treemacs
  :config
  (setq treemacs-width 40)
  :bind
  (:map global-map
	([f8] . treemacs)))

(use-package undo-tree
  :demand t
  :config
  (setq evil-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

;; (use-package pomidor
;;   :config
;;   (setq pomidor-play-sound-file
;; 	(lambda (file)
;; 	  (start-process "aplay" nil "aplay" file))))

(use-package vterm :defer)

(use-package evil-mc :defer)

(use-package writeroom-mode
  :init
  (setq writeroom-restore-window-config t)
  (setq writeroom-width 100))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "M-p"))
  :init
  (persp-mode))

(use-package esup
  :defer
  :config
  (setq esup-depth 0))
