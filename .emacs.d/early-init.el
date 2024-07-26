(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(blink-cursor-mode 0)
(setq ring-bell-function 'ignore) ; this is actually sound, but...

(toggle-frame-fullscreen)

(load-theme 'modus-vivendi)
