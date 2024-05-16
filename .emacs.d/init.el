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

(column-number-mode 1) ;; TODO

(setq display-line-numbers-type 'relative)

(dolist (mode '(text-mode-hook
               prog-mode-hook
               conf-mode-hook))
  (add-hook mode #'display-line-numbers-mode))

(visual-line-mode 1)

(setq scroll-step 1)
(setq scroll-margin 10)
(setq scroll-conservatively 1000)
(setq scroll-preserve-screen-position 1)

(set-face-attribute 'default nil :height 140)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-tomorrow-day t))

(add-hook 'prog-mode '(setq show-trailing-whitespace t))

(setq warning-minimum-level :emergency)

(defalias 'yes-or-no-p 'y-or-n-p)

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

(electric-pair-mode 1)

(recentf-mode 1)

(desktop-save-mode 1)

(save-place-mode 1)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t) ; for dired

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package pdf-tools
  :config
  (pdf-tools-install))

(use-package org-bullets)

(add-hook 'org-mode-hook (lambda()
                             (org-bullets-mode 1)
                             (org-indent-mode 1)
                             (set-face-attribute 'org-document-title nil :height 1.8)
                             (set-face-attribute 'org-level-1 nil :height 1.8)
                             (set-face-attribute 'org-level-2 nil :height 1.5)
                             (set-face-attribute 'org-level-3 nil :height 1.2)
                             (org-overview)))

(setq org-hide-emphasis-markers t)

; org mode lists
; (font-lock-add-keywords 'org-mode
;     '(("^ *\\([-]\\) "
;     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package eglot
  :hook
  (before-save . eglot-format)

  :init
  ;; do not block when loading lsp
  (setq eglot-sync-connect nil)

  ;; don't use more than one line for eldoc, unless called with K
  (setq eldoc-echo-area-use-multiline-p 1)

  (define-key evil-normal-state-map (kbd "gi") 'eglot-find-implementation))

(use-package go-mode
  :hook
  (go-mode . eglot-ensure))

(use-package nix-mode)

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

(use-package corfu
  :init
  (corfu-auto t) ; automatically pops up as you type
  (corfu-auto-delay 200)
  (corfu-auto-prefix 1)
  (global-corfu-mode))

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-secondary-delay 0.1))

(use-package magit)

(use-package diff-hl
  :init (global-diff-hl-mode 1))

;; (use-package neotree
;;   :config
;;   (global-set-key [f8] 'neotree-toggle))

(use-package treemacs
  :demand t
  :config
  (setq treemacs-width 40)
  :bind
  (:map global-map
	([f8] . treemacs)))

(use-package vertico
  :config
  (vertico-mode 1)
  (keymap-set vertico-map "C-j" #'vertico-next)
  (keymap-set vertico-map "C-k" #'vertico-previous))

(use-package undo-tree
  :init (global-undo-tree-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; (use-package pomidor
;;   :config
;;   (setq pomidor-play-sound-file
;; 	(lambda (file)
;; 	  (start-process "aplay" nil "aplay" file))))
