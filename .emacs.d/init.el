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

(blink-cursor-mode 0)
(setq ring-bell-function 'ignore) ; this is actually sound, but...

(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

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

(setq-default show-trailing-whitespace t)

(use-package org)
(use-package org-bullets
    :after org)

(add-hook 'org-mode-hook #'(lambda()
			    (org-bullets-mode 1)
			    (set-face-attribute 'org-document-title nil :height 1.8)
			    (set-face-attribute 'org-level-1 nil :height 1.8)
			    (set-face-attribute 'org-level-2 nil :height 1.5)
			    (set-face-attribute 'org-level-3 nil :height 1.2)))

(setq org-hide-emphasis-markers t)

; org mode lists
; (font-lock-add-keywords 'org-mode
;     '(("^ *\\([-]\\) "
;     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(setq custom-file "~/Git/dotfiles/.emacs.d/custom.el")

(savehist-mode 1)
(setq history-length 25)

(electric-pair-mode 1)

(recentf-mode 1)

(desktop-save-mode 1)

(save-place-mode 1)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t) ; for dired

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package eglot
    :hook
    (go-mode . eglot-ensure)
    :config
    (keymap-set eglot-mode-map "C-x r" #'eglot-rename))

; format on save
(add-hook 'before-save-hook 'eglot-format)

(use-package go-mode)

(use-package nix-mode)

(use-package evil
    :demand t
    :init
    (setq evil-want-C-u-scroll t) ; C-u won't work by default
    (setq evil-want-keybinding nil) ; what? idk
    :config
    (evil-mode 1))

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
    :custom
    (corfu-auto t) ; automatically pops up as you type
    :init
    (global-corfu-mode))

(use-package which-key
    :config (which-key-mode))

(use-package magit)

(use-package diff-hl
:demand t
:config (diff-hl-mode 1))

(use-package neotree
    :config
    (global-set-key [f8] 'neotree-toggle))

(use-package vertico
    :config
    (vertico-mode 1)
    (keymap-set vertico-map "C-j" #'vertico-next)
    (keymap-set vertico-map "C-k" #'vertico-previous))

(use-package restart-emacs)
