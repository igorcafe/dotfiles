(setenv "PATH" (concat (getenv "PATH") "/home/user/go/bin"))

; reduce visual clutter
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)

; flash when the bell rings
(setq visible-bell t)
(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)

; always show line numbers + relative numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

; change font size
(set-face-attribute 'default nil :height 140)

; looks like vscode light theme
(load-theme 'modus-vivendi)

; recent files with M-x recentf-open-files
(recentf-mode 1)

; save history of minibuffers
(setq history-length 25)
(savehist-mode 1)

; save cursor position
(save-place-mode 1)

; refresh buffers when files are changed in disk
(global-auto-revert-mode 1)

; useful for refreshing dired
(setq global-auto-revert-non-file-buffers t)

; escape to quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setq use-package-always-ensure t)

; install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

(use-package key-chord
  :config
  (key-chord-mode 1))

; vim-like bindings
(use-package evil
  :after key-chord
  :demand t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  :config
  (setq key-chord-two-keys-delay 0.2)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq evil-want-integration t)
  (evil-collection-init))

; lsp builtin client
(use-package eglot
  :hook
  (go-mode . eglot-ensure))

; autosuggestions
(use-package corfu
  :custom
  (corfu-auto t)
  :init
  (global-corfu-mode))

; languages
(use-package go-mode)
(use-package nix-mode)

(use-package which-key
  :config (which-key-mode))

(use-package vertico
  :config (vertico-mode 1))

(use-package magit)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit corfu nix-mode vertico which-key use-package lsp-mode key-chord ivy go-mode evil-collection command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
