; reduce visual clutter
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)

; flash when the bell rings
(setq visible-bell t)

; always show line numbers
(global-display-line-numbers-mode 1)

; change font size
(set-face-attribute 'default nil :height 140)

; looks like vscode light theme
(load-theme 'leuven)

; escape quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
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

(use-package evil
  :after key-chord
  :demand t
  :init
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

(use-package ivy
  :bind (:map ivy-minibuffer-map
  ("TAB" . ivy-alt-done)
  ("C-j" . ivy-next-line)
  ("C-k" . ivy-previous-line))
  :config
  (ivy-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ivy command-log-mode evil-collection key-chord use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
