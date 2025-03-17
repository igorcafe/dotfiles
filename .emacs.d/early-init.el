;; Early Init

;; Hides startup message, scroll bar and tool bar.


(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)



;; No annoying bell sound nor flashing lights when something goes wrong.


(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)



;; Set default theme before loading doom themes.


;;(load-theme 'modus-vivendi)



;; Increase default font size.


(set-face-attribute 'default nil :height 140)
