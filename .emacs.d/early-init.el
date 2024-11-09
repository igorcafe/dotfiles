(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)

;;(load-theme 'modus-vivendi)

(set-face-attribute 'default nil :height 140)
