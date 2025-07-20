;; reduce GC impact
(setq gc-cons-threshold (* 800000 10))

;; show GC messages
(setq garbage-collection-messages t)

;;
(require 'package)

(setq package-archives
      '(("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; setup straight.el
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name
;;         "straight/repos/straight.el/bootstrap.el"
;;         (or (bound-and-true-p straight-base-dir)
;;             user-emacs-directory)))
;;       (bootstrap-version 7))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (straight-use-package 'use-package)

;; Always use straight unless specificied not to
;; (setq straight-use-package-by-default t)

;; Don't check for modifications on startup
;; use M-x straight-rebuild-package instead
;; (setq straight-check-for-modification 'never)

;; Auto-tangle config
;; Automatically generate =init.el= and =early-init.el= when I save this file.
;; (use-package org-auto-tangle :defer t)

;; Y or N instead of Yes or No prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't show warnings, only errors
(setq warning-minimum-level :error)

;; Disable lock files
(setq create-lockfiles nil)

;; Emacs directories
(setq user-emacs-directory "~/.emacs.d/")

;; Disable backup files
(setq make-backup-files nil)

;; TODO Auto-save files
(setq auto-save-file-name-transforms
  `((".*" "~/.cache/emacs/" t)))

;; Customization information file
(setq custom-file "~/.emacs.d/custom.el")

;; Benchmark init
;; I was getting very bad startup times so I added this just to be sure.
(use-package esup
  :config
  (setq esup-depth 0))

;; Treat underline as part of the word
(use-package emacs
  :hook ((fundamental-mode . (lambda ()
                               (modify-syntax-entry ?_ "w")))))

;; Show column number in modeline
(column-number-mode 1)

;; Show line numbers
;; Enable line numbers for some modes.
(use-package emacs
  :hook ((text-mode
          prog-mode
          conf-mode
          go-dot-mod-mode
          restclient-mode) . display-line-numbers-mode))

;; Absolute and relative line numbers
;; Show absolute line numbers for insert state and relative for others.
(defun my/evil-display-line-numbers ()
  (when display-line-numbers
    (if (eq evil-state 'insert)
        (setq display-line-numbers-type t)
      (setq display-line-numbers-type 'relative))
    (display-line-numbers-mode 1)))

(use-package emacs
  :after evil
  :hook ((evil-insert-state-entry
          evil-normal-state-entry
          evil-visual-state-entry)
         . my/evil-display-line-numbers))

;; Truncate long lines
(set-default 'truncate-lines t)

;; Indent using 2 spaces
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Refresh buffers on change
;; Refreshs file automatically when its changed by other program.
(use-package emacs
  :config
  (global-auto-revert-mode 1))

;; persistent scratch file
(add-hook 'emacs-startup-hook
          (lambda ()
            (find-file (expand-file-name "scratch.el" user-emacs-directory))))

;; function to quickly delete file associated with current buffer
;; and killing the buffer... use with cautious!
(use-package emacs
  :after (evil)
  :config
  (defun my/current-buffer-delete ()
    (interactive)
    (delete-file (buffer-file-name))
    (kill-buffer (current-buffer)))
  :bind (:map evil-normal-state-map
        ("SPC b X" . my/current-buffer-delete)))

;; Use =ibuffer= (builtin) instead of list-buffers.
(use-package emacs
  :bind ("C-x C-b" . ibuffer))

;; Persist minibuffer's history
;; In ~M-x~, ~C-x C-f~ and so on.
(savehist-mode 1)
(setq history-length 100)

;; Save emacs state
;; (desktop-save-mode 1)

;; Display date, time and battery in modeline
(use-package emacs
  :after doom-modeline
  :config
  (setq display-time-day-and-date t)
  (setq display-time-format "%a %H:%M ")
  (setq display-time-default-load-average nil)
  (display-time-mode 1)
  (display-battery-mode 1))

;; evil-mode - vim mode emulation
;; evil mode and evil-collection provide vim-like bindings.
(use-package evil
  :defer 1
  :custom
  (evil-want-integration t)
  (evil-want-C-u-scroll t)
  (evil-want-keybinding nil) ; what? idk
  ;;(evil-want-minibuffer t)
  (evil-undo-system 'undo-redo)
  (evil-cross-lines t)
  :bind
  (("C-x C-h" . previous-buffer)
   ("C-x C-l" . next-buffer)
   ("C-x C-u" . universal-argument)
   ("C-d" . nil)
   :map evil-insert-state-map
   ("C-a" . nil)
   ("C-e" . nil)
   ("C-r" . nil)
   ("C-d" . nil)
   ("C-y" . nil)
   :map evil-motion-state-map
   ("C-w >" . (lambda () (interactive) (evil-window-increase-width 10)))
   ("C-w <" . (lambda () (interactive) (evil-window-decrease-width 10)))
   ("C-y" . nil)
   ("SPC" . nil)
   :map evil-normal-state-map
   ("SPC" . nil)
   ("C-#" . evil-search-word-backward)
   ("C-*" . evil-search-word-forward)
   ("#" . (lambda ()
            (interactive)
            (evil-search-word-backward 1 (thing-at-point 'symbol))))
   ("*" . (lambda ()
            (interactive)
            (evil-search-word-forward 1 (thing-at-point 'symbol)))))
   ("C-y" . nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; evil-surround - surround text with parenthesis, quotes, and so on
;; Works exactly like you-surround.
(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

;; key-chord - time-based keymaps for evil
;; I only use it to map ~jk~ to ~<Escape>~, aka switch to normal mode.
(use-package key-chord
  :after evil
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

;; hl-line-mode - highlight current line
(use-package emacs
  :config
  (global-hl-line-mode 1))

;; all-the-icons + all-the-icons-dired - icon packages
(use-package all-the-icons
  :after doom-modeline)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(defun all-the-icons-dired-mode (&rest args))
;; run once
;;(all-the-icons-install-fonts t)
;;(nerd-icons-install-fonts t)

;; doom-themes - nice themes
(use-package doom-themes
  :defer 0.3
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-one t))

;; cycle between favorite theme
(use-package emacs
  :config
  (defvar favorite-themes '(doom-one-light doom-one))

  (defun cycle-favorite-themes ()
    (interactive)
    (let* ((current (car custom-enabled-themes))
           (i-current (cl-position current favorite-themes))
           (i-next (% (+ i-current 1) (length favorite-themes)))
           (theme (nth i-next favorite-themes)))
      (load-theme theme t))))


(use-package snake
  :ensure nil
  :config
  (setq snake-initial-velocity-x 1)
  (setq snake-dot-options '(((glyph colorize) (t 42))
                       ((color-x color-x) (mono-x grid-x) (color-tty color-tty))
                       (((glyph color-x) [1 0.2 0.1]) (color-tty "red"))))
  (setq snake-snake-options '(((glyph colorize) (emacs-tty 79) (t 32))
                         ((color-x color-x) (mono-x mono-x) (color-tty color-tty)
                          (mono-tty mono-tty))
                         (((glyph color-x) [0.3 0.7 0.1]) (color-tty "green"))))
  (setq snake-border-options '(((glyph colorize) (t 43))
                         ((color-x color-x) (mono-x grid-x) (color-tty color-tty))
                         (((glyph color-x) [0.5 0.5 0.5]) (color-tty "white"))))
  (setq snake-blank-options '(((glyph colorize) (t 32))
                         ((color-x color-x) (mono-x grid-x) (color-tty color-tty))
                         (((glyph color-x) [0.2 0.2 0.1]) (color-tty "black")))))

;; automatically change to light/dark theme following system theme
(use-package dbus
  :ensure nil
  :config
  (defun my/handle-dbus-setting-changed (a setting values)
    (when (string= setting "ColorScheme")
      (let ((scheme (car values)))
        (cond
         ((string-match-p "Dark" scheme)
          (load-theme 'doom-one t))
         ((string-match-p "Light" scheme)
          (load-theme 'doom-one-light t))
         (t
          (warn "I don't know how to handle this ColorScheme: %s" scheme))))))

  (dbus-register-signal :session
                        "org.freedesktop.portal"
                        "/org/freedesktop/portal/desktop"
                        "org.freedesktop.impl.portal.Settings"
                        "SettingChanged"
                        #'my/handle-dbus-setting-changed))

;; doom-modeline - nice modeline
(use-package doom-modeline
  :defer 1.2
  :config
  ;; (setq doom-modeline-buffer-name nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-version nil)
  (setq doom-modeline-lsp nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-lsp-icon nil)
  (setq doom-modeline-env-enable-python nil)
  (setq doom-modeline-persp-name nil)
  (setq doom-modeline-persp-icon nil)
  (doom-modeline-mode 1))

;; flymake (builtin) - syntax checking
(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-flymake-display-mode 'line)
  (setq sideline-backends-right '(sideline-flymake)))

;; eletrict-pair-mode (builtin) - auto close pairs based on mode
(electric-pair-mode 1)

;; keybindings for adding pairs -> ' " [ { (
(use-package emacs
  :after evil
  :config
  (dolist (pair '(("\"" "\"")
                  ("'" "'")
                  ("[" "]")
                  ("{" "}")
                  ("<" ">")))
    (eval `(bind-key ,(concat "M-" (car pair))
                     (lambda ()
                       (interactive)
                       (insert-pair nil ,(car pair) ,(car (last pair))))
                     evil-visual-state-map))))

;; compile - run commands and easily follow errors

(defun my/compilation-buffers (&optional project)
  (mapcar (lambda (buf)
            (buffer-name buf))
          (seq-filter
           (lambda (buf)
             (with-current-buffer buf
               (and (eq major-mode 'compilation-mode)
                    (or (eq project nil)
                        (string= (project-root project)
                                 (project-root (project-current)))))))
           (buffer-list))))

(defun my/compilation-buffer-name (name project)
  (if project
      (format "*[%s] compilation - %s*" (project-name project) name)
    (format "*compilation - %s*" name)))

(defun my/compile-switch-to-buffer (&optional project)
  (interactive)
  (let ((buffer (completing-read "Compilation buffer: "
                                 (my/compilation-buffers project)
                                 nil
                                 t)))
    (switch-to-buffer buffer)))

(defun my/compile-switch-to-project-buffer (project)
  (interactive (list (project-current t)))
  (my/compile-switch-to-buffer project))


(defun my/compile (cmd-or-buffer &optional project name)
  (interactive (list (completing-read "[Re]compile: "
                                (my/compilation-buffers))))
  (let* ((buffer (get-buffer cmd-or-buffer))
         (default-directory (if project
                                (project-root project)
                              default-directory))
         (compilation-save-buffers-predicate
          (lambda ()
            (string-prefix-p default-directory
                             (buffer-file-name)))))
    (if buffer
        (with-current-buffer buffer
          (recompile))
      (compilation-start
       cmd-or-buffer
       nil
       (eval `(lambda (_)
                ,(my/compilation-buffer-name (or name cmd-or-buffer) project)))))))

(defun my/project-compile (cmd-or-buffer &optional project name)
  (interactive (list (completing-read "Project [re]compile: "
                                      (my/compilation-buffers))
                     (project-current t)))
  (my/compile cmd-or-buffer project name))

(use-package compile
  :ensure nil
  :defer
  :init
  ;; :hook
  ;; (compilation-filter . ansi-color-compilation-filter)
  :config
  (setq compilation-scroll-output t)
  (setq compilation-always-kill t))

;; project.el (builtin) - managing projects
;; Helps you manage projects based on version control systems, like
;; git repos. Check =C-x p p=.

;; custom function for launching eshell with descriptive buffer names
(defun my/project-eshell ()
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (proj-name (file-name-nondirectory
                     (directory-file-name default-directory)))
         (buffer-name (read-string "buffer name: "
                                   (concat "eshell - " proj-name)))
         (eshell-buffer (get-buffer buffer-name)))
    (if eshell-buffer
        (pop-to-buffer eshell-buffer)
      (with-current-buffer (eshell t)
        (rename-buffer (concat "*" buffer-name "*"))))))

;; Replace default project grep with consult
(defun my/consult-git-grep ()
  (interactive)
  (consult-git-grep nil (thing-at-point 'symbol)))

;; Customize project.el commands.
(use-package project
  :config
  (setq project-switch-commands
        '((project-find-file "Find file" ?f)
          (my/consult-git-grep "Find regexp" ?g)
          (project-find-dir "Find directory" ?d)
          (eat-project "Eat" ?t)
          (consult-project-buffer "Buffers" ?b)
          (my/project-eshell "Eshell" ?e)
          ;; (my/project-compile "Compile" ?c)
          (magit-project-status "Magit" ?m)))
  :bind
  (:map project-prefix-map
        ("t" . eat-project)
        ("e" . my/project-eshell)
        ("b" . consult-project-buffer)
        ("c" . my/project-compile)
        ("C" . my/compile-switch-to-project-buffer)
        ("g" . my/consult-git-grep)
        ("m" . magit-project-status)))

;; go-mode - Go support
(use-package go-mode :defer t)

;; go-tag - automatically adding/removing struct tags
(use-package go-tag :defer t)

;; nix-mode - Nix support
(use-package nix-mode :defer t)

;; typescript-mode - TypeScript support
(use-package typescript-mode
  :defer t
  :config
  (setq typescript-indent-level 2))

;; yaml-mode - YAML support
(use-package yaml-mode :defer t)

;; markdown-mode - Markdown support
(use-package markdown-mode :defer t)

;; nxml-mode (builtin) - XML support
(use-package emacs
  :config
  (setq nxml-child-indent 4)
  (setq nxml-attribute-indent 4))

;; Enhanced Web and Vue support
(use-package web-mode
  :mode ("\\.vue\\'" . vue-mode)
  :init
  (define-derived-mode vue-mode web-mode "Vue")
  (setq web-mode-script-padding 0
        web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2))

;; c-mode (builtin) - C/C++ support
(use-package emacs
  :hook (c-mode . (lambda ()
                    (setq c-basic-offset 2)
                    (setq indent-tabs-mode nil))))

;; eglot (builtin) - LSP client
;; Eglot is a builtin LSP (Language Server Protocol) client for emacs.

(defun my/eglot-format-on-save ()
  (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
    (ignore-errors (call-interactively 'eglot-format))
    (ignore-errors (call-interactively 'eglot-code-action-organize-imports))))

(use-package eglot
  :after evil
  :hook
  ;; before saving, if eglot is enabled, try to format and organize imports
  ((before-save . my/eglot-format-on-save)

   ;; start eglot only if file is somewhere in home (avoid /nix/store and similar)
   (prog-mode . (lambda ()
                  (when (string-prefix-p (getenv "HOME") (buffer-file-name))
                      (eglot-ensure)))))

  :bind
  (:map evil-normal-state-map
        ("gi" . eglot-find-implementation)
        ("gy" . eglot-find-typeDefinition)
        ("SPC l r" . eglot-rename)
        ("SPC l R" . eglot-reconnect)
        ("SPC l a a" . eglot-code-actions)
        ("SPC l a e" . eglot-code-action-extract))
  :init
  ;; do not block when loading lsp
  (setq eglot-sync-connect nil))

;; eldoc (builtin) - showing documentation of symbols
;; It also retrieves data from =eglot=.
(use-package emacs
  :config
  (setq eldoc-echo-area-use-multiline-p 1))

;; eldoc-box - eldoc in a box below cursor
;; I use eldoc-box to show docs as a hover box instead of using echo area.
(use-package eldoc-box
  :hook (eldoc-mode . eldoc-box-hover-at-point-mode)
  :after evil
  :config
  :bind
  (:map evil-normal-state-map
        ("K" . eldoc-box-help-at-point)))

;; company - completion popup like VS Code's
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-tooltip-limit 10)
  (company-idle-delay 0.15)
  (company-minimum-prefix-length 2)
  (company-selection-wrap-around t)
  (company-require-match 'never)
  :bind
  ((:map global-map
         ("C-SPC" . company-complete))
   (:map company-active-map
         ("TAB" . nil)
         ("<backtab>" . nil))))

;; yasnippet - snippets
(use-package yasnippet
  :hook ((prog-mode . yas-minor-mode)
         (eglot-managed-mode
          . (lambda ()
              (setq company-backends '((company-capf :with company-yasnippet))))))
  :bind (:map yas-minor-mode-map
              ("TAB" . nil)
              ("M-i" . yas-expand))
  :init
  (defun my/yas-auto-expand ())

  :config
  (yas-define-snippets
   'typescript-mode
   '(("lv" "console.log(\"${1:var}:\", $1)")))
  (yas-define-snippets
   'go-mode
   '(("erris" "errors.Is(err, $1)")
     ("erras" "errors.As(err, &$1)")
     ("iferr" "if err != nil {\n\treturn err\n}")
     ("errf" "fmt.Errorf(\"$0: %w\", err)")
     ("co" "const ${1:name} = ${2:value}")
     ("wr" "${1:w} http.ResponseWriter, ${2:r} *http.Request")
     ("hf" "http.HandleFunc(\"/$1\", func(w http.ResponseWriter, r *http.Request) {\n\t$0\n}")
     ("hand" "func $1(w http.ResponseWriter, r *http.Request) {\n\t$0\n}")
     ("herr" "http.Error(w, ${1:err.Error()}, ${3:http.StatusInternalServerError})")
     ("go" "go func($1) {\n\t$0\n}($2)")
     ("func" "func $1($2) $3 {\n\t$0\n}")
     ("meth" "func (${1:receiver} ${2:type}) ${3:method}($4) $5 {\n\t$0\n}")
     ("tyf" "type ${1:name} func($3) $4")
     ("tyi" "type ${1:name} interface {\n\t$0\n}")
     ("tys" "type ${1:name} struct {\n\t$0\n}")
     ("pkgm" "package main\n\nfunc main() {\n\t$0\n}")
     ("switch" "switch ${1:expression} {\ncase ${2:condition}:\n\t$0\n}")
     ("sel" "select {\ncase ${1:condition}:\n\t$0\n}")
     ("cs" "case ${1:condition}:$0")
     ("for" "for ${1:i} := ${2:0}; $1 < ${3:count}; $1${4:++} {\n\t$0\n}")
     ("forr" "for ${1:_}, ${2:v} := range ${3:v} {\n\t$0\n}")
     ("fp" "fmt.Println(\"$1\")")
     ("ff" "fmt.Printf(\"$1\", ${2:var})")
     ("pv" "fmt.Printf(\"${1:var}: %#+v\\\\n\", ${1:var})")
     ("lp" "log.Println(\"$1\")")
     ("lf" "log.Printf(\"$1\", ${2:var})")
     ("lv" "log.Printf(\"${1:var}: %#+v\\\\n\", ${1:var})")
     ("var" "var ${1:name} ${2:type}")
     ("tf" "func Test$1(t *testing.T) {\n\t$0\n}")
     ("bf" "func Benchmark$1(b *testing.B) {\n\tfor i := 0; i < b.N; i++ {\n\t\t$0\n\t}\n}"))))

;; envrc - direnv integration
;; Works better than =direnv-mode= for me.
(use-package envrc
  :defer 0.5
  :config
  (envrc-global-mode))

;; aider
(use-package aider
  :vc (:url "https://github.com/tninja/aider.el.git")
  :config
  (setq eshell-scroll-to-bottom-on-input t)
  (setq aider-args
        '("--model" "gpt-4o"
          "--no-auto-commits"))
  :bind
  ("C-c i" . aider-transient-menu))

;; recentf-mode (builtin) - persistent history of recent files
;; Show recent files with `C-x C-r'.
(use-package recentf
  :ensure nil
  :init
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 100)
  (recentf-mode 1)
  :bind ("C-x C-r" . consult-recent-file))

;; use-package org to avoid any bugs
(use-package org :defer t)

;; olivetti - horizontal paddings for windows
(use-package olivetti
  :hook (((prog-mode
          go-dot-mod-mode
          eww-mode
          text-mode
          conf-mode
          org-agenda-mode
          restclient-mode)
          . olivetti-mode)
         (olivetti-mode . (lambda () (toggle-truncate-lines 1))))
  :init
  (setq-default olivetti-body-width 100)
  :config
  (remove-hook 'olivetti-mode-on-hook 'visual-line-mode))

;; save-place-mode - save cursor position per file
(save-place-mode 1)

;; consult - multiple search utilities
(use-package consult
  :after evil
  :bind
  (("C-x b" . consult-buffer)
   :map evil-normal-state-map
   ;; analogous to project-find-regexp
   ("SPC p g" . consult-git-grep)

   ;; buffer errors
   ("SPC b e" . consult-flymake)

   ;; find line
   ("SPC b l" . consult-line)

   ;; "paste from clipboard"
   ("M-y" . consult-yank-from-kill-ring)

   ;; commands by mode
   ("M-X" . consult-mode-command)

   ;; buffer definitions
   ("SPC b d" . consult-imenu)))

;; tab-line (builtin) - show buffers as tabs
;; It works per window, showing the recent buffers you opened in that window.
(use-package emacs
  :config
  (setq tab-line-switch-cycling t))

;; tab-bar (builtin) - tabs like vim
;; I use it just to make 2 or 3 different "window layouts" and switch
;; between them
;; (use-package emacs
;;   :config
;;   (setq tab-bar-show nil))

;; bookmarks: hack to remember last point position
;; by default it always jump to the place where you saved the bookmark
;; using this it will jump to the place where I was the last time
(use-package emacs
  :bind ("C-x r b" . (lambda ()
                       (interactive)
                       (find-file (bookmark-location
                                   (bookmark-completing-read
                                    "Jump to bookmark"
                                    bookmark-current-bookmark))))))


;; perspective - "isolated" workspaces
(use-package perspective
  :bind (("C-c p p" . persp-switch)
         ("C-c p k" . persp-remove-buffer)
         ("C-c p x" . my/persp-kill-current)
         ("C-c p X" . persp-kill)
         ("C-c p r" . persp-rename)
         ("C-c p b" . persp-switch-to-buffer)
         ("C-c p a" . persp-add-buffer)
         ("C-c p A" . persp-set-buffer)
         ("C-c p m" . persp-merge)
         ("C-c p u" . persp-unmerge))
  :init
  ;; I don't want a prefix key
  (setq persp-suppress-no-prefix-key-warning t)
  (setq persp-initial-frame-name "temp")

  (defun my/persp-kill-current ()
    (interactive)
    (persp-kill (persp-name (persp-curr))))

  :config
  (persp-mode 1)
  (use-package consult)

  ;; keep previous/next buffer history per perspective
  (setq switch-to-prev-buffer-skip
        (lambda (win buff bury-or-kill)
          (not (persp-is-current-buffer buff))))

  ;; consult integration
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)

  ;; thats how I managed to do it lul
  (setq persp-sort 'created)
  (dolist (num '(1 2 3 4 5 6 7 8 9))
    (eval `(bind-key ,(format "M-%d" num)
                     (lambda ()
                       (interactive)
                       (persp-switch-by-number ,num))))))

;; whitespace (builtin) - show whitespaces as symbols
(use-package whitespace
  :hook
  ((prog-mode conf-mode) . whitespace-mode)
  :config
  (setq whitespace-style '(face tabs spaces trailing space-mark tab-mark)))

;; restclient - http client
(use-package restclient
  :defer t
  :mode ("\\.http\\'" . restclient-mode))

;; simple-httpd - static http server
(use-package simple-httpd :defer t)

;; winner-mode (builtin) - undo/redo window changes
(use-package emacs
  :after evil
  :bind
  (:map evil-normal-state-map
        ("C-w u" . winner-undo)
        ("C-w C-r" . winner-redo))
  :config
  (winner-mode 1))

;; dired (builtin) - file manager
;; - Hide details by default (show only filename + icon with =all-the-icons-dired=)
;; - Rename buffer to "dired - <path>"
(use-package dired
  :ensure nil
  :preface
  (defun my/dired-rename ()
    (rename-buffer (format "dired - %s" dired-directory)))
  (defun my/dired-xdg-open ()
    (interactive)
    (browse-url-xdg-open (dired-get-filename)))
  :hook
  ((dired-mode . dired-hide-details-mode)
   (dired-mode . my/dired-rename))
  :config
  (setq global-auto-revert-non-file-buffers t)
  (setq dired-omit-files "^\\.")
  ;; :bind
  ;; (:map dired-mode-map
  ;;       ("S-TAB" . dired-find-file-other-window))
  :bind
  (:map dired-mode-map
        ("<normal-state> g x" . my/dired-xdg-open)
        ("M-o" . dired-omit-mode)))

;; dired-subtree - add subtree view to dired
(use-package dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
        ("TAB" . dired-subtree-toggle)))

;; dired-preview - preview file contents (including images)
(use-package dired-preview
  :after dired
  :defer t
  :preface
  (defun my/dired-preview-at-right ()
    '((display-buffer-in-side-window)
      (side . right)
      (window-width . 0.5)))
  :config
  (setq dired-preview-delay 0.3)
  (setq dired-preview-display-action-alist #'my/dired-preview-at-right))

;; magit - git interface
;; I use the default ~C-x g~ binding.
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (magit-diff-refine-hunk 'all)
  (transient-display-buffer-action '(display-buffer-below-selected))
  :bind
  ("C-x g" . magit)
  (:map magit-section-mode-map
        ("M-1" . nil)
        ("<normal-state> M-1" . nil)
        ("M-2" . nil)
        ("<normal-state> M-2" . nil)
        ("M-3" . nil)
        ("<normal-state> M-3" . nil)
        ("M-4" . nil)
        ("<normal-state> M-4" . nil)))

;; ediff (builtin) - diff files, commits, and so on
(use-package ediff
  :ensure nil
  :config
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally))

;; diff-hl - highlight uncommited changes
(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (after-save . diff-hl-update))
  :config
  (setq diff-hl-show-staged-changes nil)
  (global-diff-hl-mode 1))

;; blamer-mode - git blame like gitlens
(use-package blamer :defer t)

;; whick-key - suggests key combinations as you press them.
(use-package which-key
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))

;; vterm - integrated terminal
;; Launch vterm with a custom buffer name.
;; (defun my/vterm (name)
;;   (interactive "sname: ")
;;   (vterm (concat "vterm - " name)))

;; (use-package vterm
;;   :ensure nil
;;   :after evil
;;   :bind
;;   ((:map evil-normal-state-map
;;          (("SPC t" . my/vterm)))
;;    (:map vterm-mode-map
;;          (("M-1" . nil)
;;           ("M-2" . nil)))))



;; eat- Emulate A Terminal: terminal emulator
(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat.git")
  :hook ((eshell-load . eat-eshell-mode)
         (eshell-load . eat-eshell-visual-command-mode)
         (eat-mode . compilation-shell-minor-mode))
  :bind
  (:map eat-mode-map
        ("C-y" . eat-yank))
  (:map eat-eshell-semi-char-mode-map
        ("M-1" . nil)
        ("M-2" . nil)
        ("M-3" . nil)
        ("M-4" . nil)))


;; vertico - vertical completion
;; Improves minibuffer by showing multiple options in a vertical list.
(use-package vertico
  :defer 0.4
  :config
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (setq vertico-count 20)
  (setq vertico-cycle t)
  (setq vertico-sort-function 'vertico-sort-history-alpha)

  :bind
  (:map vertico-map
        ("C-j" . vertico-next)
        ("C-k" . vertico-previous)))

;; marginalia - show more data for help functions
;;
;; - Adds description for commands in ~M-x~
;; - Adds extra info to find file
;; - Adds extra info to ~C-h v~
(use-package marginalia
  :hook (after-init . marginalia-mode))

;; orderless - fuzzy completion
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; emms - music player
(use-package emms
  :after evil
  :config
  (emms-all)
  (emms-default-players)

  ;; all my songs are downloaded from youtube with org-music and don't
  ;; have metadata.
  ;; so emms always show the full file path in the modeline, which is always
  ;; "path/to/songs/Author - Song Name.m4a"
  ;; this function replaces it by only "🎵 Song Name"
  (setq emms-mode-line-mode-line-function
        ;; (lambda ()
        ;;   (let* ((path (emms-track-description
        ;;                 (emms-playlist-current-selected-track)))
        ;;          (song (when (string-match ".*? - \\(.*\\)\\.m4a$" path)
        ;;                  (match-string 1 path))))
        ;;     (format "🎵 %s  " song)))
        nil)
  :bind
  (:map global-map
        ("C-c m j" . mpv-playlist-next)
        ("C-c m k" . mpv-playlist-prev)
        ("C-c m ," . mpv-seek-backward)
        ("C-c m ." . mpv-seek-forward)
        ("C-c m SPC" . mpv-pause)
        ("C-c m s" . mpv-kill)
        ("C-c m e" . mpv-jump-to-playlist-entry)
        ("C-c m R" . mpv-playlist-shuffle)))

;; telega - telegram client
(use-package telega
  :ensure nil ;; installed and built through nix
  :hook (telega-mode . telega-mode-line-mode)
  :config
  (setq telega-use-images t)
  (setq telega-emoji-use-images nil)
  (setq telega-sticker-size '(8 . 48))
  (setq telega-chat-group-messages-for nil) ;; (not (or saved-messages (type channel bot)))
  (setq telega-emoji-font-family "Noto Color Emoji")
  (setq telega-video-player-command '(format "mpv"))
  (setq telega-chat-input-markups '("markdown2" "org"))
  :bind
  ((:map global-map
        ("C-c g g" . telega)
        ("C-c g b" . telega-switch-buffer))
   (:map telega-msg-button-map
        ("SPC" . nil))))

;; elfeed - client for Atom and RSS feeds
(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds
        '(
          ;; DHH
          ("https://world.hey.com/dhh/feed.atom")

          ;; Martin Fowler
          ("https://martinfowler.com/feed.atom")

          ;; Go Blog
          ("https://go.dev/blog/feed.atom" golang)

          ;; Lakka News
          ("https://www.lakka.tv/articles/feed.xml")

          ;; Igor Melo (dev.to)
          ("https://dev.to/feed/igormelo")

          ;; Things of Interest - Blog
          ("https://qntm.org/rss.php?blog")

          ;; Jesse Li
          ("https://blog.jse.li/index.xml")

          ;; Planet Emacslife
          ("https://planet.emacslife.com/atom.xml" emacs)

          ("https://g1.globo.com/dynamo/economia/rss2.xml")

          ;; ThePrimeTime
          ;;"https://www.youtube.com/feeds/videos.xml?channel_id=UCUyeluBRhGPCW4rPe_UvBZQ"
          ;; Mental Outlaw
          ;;"https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA"
          ;; Fireship
          ;;"https://www.youtube.com/feeds/videos.xml?channel_id=UCsBjURrPoezykLs9EqgamOA"
          )))

;; pdf-tools - read PDFs in emacs
;; I tried default emacs doc-view-mode but it didn't work with the PDFs I tested.
(use-package pdf-tools
  :hook (pdf-view-mode . pdf-view-themed-minor-mode)
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))

;; nov-mode - read EPUBs in emacs
(use-package nov :defer t
  :mode ("\\.epub\\'" . nov-mode))

;; auth-sources - "password manager"
(use-package auth-sources
  :ensure nil
  :defer t
  :config
  (setq auth-sources '("~/.authinfo.gpg")))

(defun auth-get-password (host login)
  (let* ((entry (car (auth-source-search
                        :host host
                        :login login
                        :require '(:secret))))
         (val (plist-get entry key)))
    (if (functionp val)
        (funcall val)
      val)))

;; pinentry - for entrying pin for gpg
(use-package pinentry
  :defer 2
  :custom
  (epg-pinentry-mode 'loopback)
  :config
  (pinentry-start))

;; gnus - email client, news reader, maybe
(use-package gnus
  :ensure nil
  :defer t
  :hook (gnus-after-getting-new-news . gnus-notifications)
  :custom
  (send-mail-function 'smtpmail-send-it)
  (smtpmail-smtp-server "smtp.gmail.com")
  (smtpmail-smtp-service 587)
  (user-full-name "Igor Melo")
  (user-mail-address "imelodev@gmail.com")
  (message-directory "~/public/mail")
  (mail-source-directory message-directory)
  (gnus-home-directory (expand-file-name "gnus" user-emacs-directory))
  (gnus-directory (expand-file-name "news" gnus-home-directory))
  (gnus-article-save-directory gnus-directory)
  (gnus-cache-directory (expand-file-name "cache" gnus-directory))
  (gnus-select-method '(nnnil))
  (gnus-secondary-select-methods
   '(
     (nnimap "gmail"
             (nnimap-address "imap.gmail.com")
             (nnimap-server-port 993)
             (nnimap-stream ssl)
             (nnimap-authinfo-file "~/.authinfo.gpg")))))

;; eww (builtin) - simple browser
;; Wrap lines instead of truncating
(use-package emacs
  :hook (eww-mode . visual-line-mode)
  :config
  ;; name buffers as [ domain ] - [ title ]
  (setq eww-readable-urls '(".*"))
  (setq eww-auto-rename-buffer "url")
  ;; (setq eww-auto-rename-buffer
  ;;       (lambda ()
  ;;         (let* ((url (plist-get eww-data :url))
  ;;                (domain (url-host (url-generic-parse-url url)))
  ;;                (title (plist-get eww-data :title)))
  ;;           (if (and title (not (string= title ""))
  ;;                      domain (not (string= domain "")))
  ;;               (format "*%s - %s*" domain title)
  ;;             (if ))
  ;;           nil)))
  )

;; gptel - GPT inside emacs
(use-package gptel
  :defer t
  :config
  (setq gptel-api-key nil))

;; mpv.el - control mpv from emacs
(use-package mpv
  :defer t
  :preface
  (defun mpv-playlist-shuffle ()
    (interactive)
    (mpv-run-command "playlist-shuffle"))
  :custom
  (mpv-default-options '("--keep-open=no")))

;; find-file launches mpv
(defvar find-file-open-in-mpv-exts
  '("ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
    "mov" "avi" "divx" "ogm" "ogv" "asf" "mkv"
    "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape"
    "flv" "webm" "aif" "opus" "spc"))

(defun find-file-open-in-mpv (orig-fun &rest args)
  (let* ((filename (car args))
         (ext (file-name-extension filename)))
      (if (member ext find-file-open-in-mpv-exts)
          (mpv-play filename)
        (apply orig-fun args))))

(advice-add 'find-file :around #'find-file-open-in-mpv)

;; org-mpv-notes - control mpv from emacs
(use-package org-mpv-notes :defer t)

;; yeetube - youtube frontend
(use-package yeetube
  :after evil

  :preface
  (defvar yeetube-org-file)

  (defun org-insert-yeetube-link ()
    (interactive)
    (let* ((last (car yeetube-history))
           (title (plist-get last :title))
           (url (plist-get last :url)))
      (if (and title url)
          (insert (message "[[%s][%s]]" url title))
        (error "no recent video found"))))

  (defun yeetube-org-find-file ()
    (require 'yeetube)
    (interactive)
    (find-file (expand-file-name yeetube-org-file org-directory)))

  (defun yeetube-org-channel-videos ()
    (require 'yeetube)
    (interactive)
    (let ((channel-id (or (org-entry-get (point) "CHANNEL")
                          (org-entry-get (point) "ITEM"))))
      (setf yeetube--channel-id channel-id)
      (yeetube-display-content-from-url
       (format "https://youtube.com/@%s/videos" channel-id))))

  :vc (:url "https://git.thanosapollo.org/yeetube")

  :init
  (setq yeetube-org-file "youtube.org")

  :config
  (evil-define-key 'normal yeetube-mode-map
    "RET" 'yeetube-play
    "M-RET" 'yeetube-search
    "C-<return>" 'yeetube-video-or-playlist-page
    "b" 'yeetube-browse-url
    "c" 'yeetube-channel-videos
    "d" 'yeetube-download-video
    "D" 'yeetube-download-change-directory
    "a" 'yeetube-download-change-audio-format
    "p" 'yeetube-mpv-toggle-pause
    "v" 'yeetube-mpv-toggle-video
    "V" 'yeetube-mpv-toggle-no-video-flag
    "s" 'yeetube-save-video
    "P" 'yeetube-play-saved-video
    "r" 'yeetube-replay
    "t" 'yeetube-view-thumbnail
    "T" 'yeetube-mpv-toggle-torsocks
    "C-q" 'yeetube-mpv-change-video-quality
    "q" 'quit-window)
  (setq yeetube-play-function #'mpv-play-url)

  :bind
  ((:map global-map
         ("C-c y s" . yeetube-search)
         ("C-c y o p" . org-insert-yeetube-link)
         ("C-c y o c" . yeetube-org-channel-videos)
         ("C-c y o F" . yeetube-org-find-file))
   (:map evil-motion-state-map
         ("RET" . nil))))

;; erc (builtin) - emacs IRC client
(use-package erc
  :ensure nil
  :defer t
  :preface
  ;; (defun my/erc-buffer-rename ()
  ;;   (rename-buffer
  ;;    (format "ERC - %s" (buffer-name))))
  ;; :hook
  ;; (erc-mode . my/erc-buffer-rename)
  :config
  (setq erc-server "irc.libera.chat"
        erc-nick "igorcafe"
        erc-autojoin-channels-alist '((Libera.Chat
                                       "#emacs"
                                       "#erc"
                                       "#go-nuts"
                                       "#newpipe"
                                       "#nixos"
                                       "#org-mode"
                                       "#systemcrafters"
                                       "#vim"
                                       ))
        erc-kill-buffer-on-part t
        erc-auto-query 'bury
        erc-log-channels-directory "~/.emacs.d/erc")

  (setq erc-fill-column 120
        erc-fill-function 'erc-fill-static
        erc-fill-static-center 20)

  (setq erc-save-buffer-on-part t
        erc-save-queries-on-quit t
        erc-log-write-after-send t
        erc-log-write-after-insert t
        erc-log-insert-log-on-open t)

  (setq erc-track-exclude '()
        erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE" "AWAY")
        erc-hide-list '("JOIN" "NICK" "QUIT" "MODE" "AWAY")
        erc-track-exclude-server-buffer t)

  (erc-log-enable))

;; erc-hl-nicks - highlight user nicks with unique color
(use-package erc-hl-nicks
  :after erc
  :init
  (add-to-list 'erc-modules 'hl-nicks))

;; Org directory
(setq org-directory "~/Sync/Org")

;; Org source block - open in the same window
;; Open ~C-c '~ in the same window
(setq org-src-window-setup 'current-window)

;; Org babel languages
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((shell . t)))
;; Org indirect buffer - open in the same window
(setq org-indirect-buffer-display 'current-window)

;; Org refile - save all buffers
(setq org-outline-path-complete-in-steps t)
(setq org-refile-targets nil)
(advice-add 'org-refile :after 'org-save-all-org-buffers)

;; Org priority - face and default value
(setq org-priority-highest ?A)
(setq org-priority-lowest ?E)
(setq org-priority-default ?D)
;; (setq org-priority-faces
;;       '((?A . (:foreground "gray"))
;;         (?B . (:foreground "gray"))
;;         (?C . (:foreground "gray"))
;;         (?D . (:foreground "gray"))
;;         (?E . (:foreground "gray"))))

;; Org tags - column
(setq org-tags-column -89)

;; Org TODOs - keywords and state logging
(use-package org
  :config
  (setq org-log-into-drawer t)
  (setq org-log-done nil)
  (setq org-log-reschedule t)
  (setq org-log-redeadline t)
  (setq org-hierarchical-todo-statistics t) ;; TODO cookie count not recursive
  (setq org-todo-keywords
        '((sequence "TODO(!)" "|" "DONE(!)")))
  (set-face-attribute 'org-headline-done nil :strike-through t :foreground "gray")
  (defun my/org-clock-in ()
    (interactive)
    (if (eq major-mode 'org-mode)
        (call-interactively 'org-clock-in)
      (call-interactively 'org-clock-in-last)))
  :bind
  ;; the keybindings are the same, just made them global
  (("C-c C-x C-i" . my/org-clock-in)
   ("C-c C-x C-o" . org-clock-out)
   ("C-c C-x C-j" . org-clock-goto)))

;; org C-c C-c on timestamp will also show (X days) for X days remaining
(use-package emacs
  :after org
  :config
  (defun my/org-timestamp-add-days-remaining (&rest args)
    (interactive)
    (let* ((line (line-number-at-pos))
           (start nil)
           (end nil)
           (now (format-time-string "%Y-%m-%d"))
           (then nil)
           (remaining nil))
      (save-excursion
        (if (looking-at "[\[<]")
            (setq start (point))
          (setq start (re-search-backward "[\[<]"))
          (when (/= line (line-number-at-pos))
            (user-error "no timestamp here")))
        (setq end (re-search-forward "[\]>]"))
        (message "%s %s" start end)
        (setq then (buffer-substring-no-properties start end))
        (setq remaining (and then
                             (days-between then now)))
        (message "%s" (if (< remaining 0)
                          (format "(%d days ago)" (abs remaining))
                        (if (= remaining 0)
                            "(today)"
                          (format "(in %d days)" remaining)))))))

  (advice-add 'org-timestamp-change :after #'my/org-timestamp-add-days-remaining))

;; Org Clock
(use-package org
  :config
  (setq org-duration-format 'h:mm)
  :bind
  (("C-c C-x C-o" . org-clock-out)
   ("C-c C-x C-j" . org-clock-goto)))

;; org-noter - annotate PDFs, EPUBs, and so on
(use-package org-noter
  :defer t
  :config
  (setq org-noter-highlight-selected-text t))

;; Org Capture - keybindings and templates
(use-package org
  :bind
  (:map global-map
        ("C-c c" . org-capture)
        ("C-c C" . org-capture-goto-last-stored))
  :config
  (setq org-capture-templates
        '(("t"
           "Create and start clocking a task"
           entry
           (file+headline "tasks.org" "Tasks")
           "* TODO %?\n%U"
           :prepend t
           :clock-in t
           :clock-keep t)
          ("d"
           "Capture to the task I'm currently [d]oing"
           entry
           (clock)
           "* %?\n%U")
          ("n"
           "Capture next action"
           entry
           (file+headline "tasks.org" "Tasks")
           "* TODO %?\n%U\n"
           :prepend t)
          ("C" "Card"
           entry
           (file "flashcards.org")
           "* TODO review card :drill:\n\n%i%?"
           :empty-lines-before 1
           :before-finalize org-id-get-create)
          ("c" "Card with source link"
           entry
           (file "flashcards.org")
           "* TODO review card :drill:\n\n%i%?\n\n[[%L][%(buffer-name (nth 1 (buffer-list)))]]"
           :empty-lines-before 1
           :before-finalize org-id-get-create)
          ("j" "Journal"
           entry
           (file+olp+datetree "journal.org")
           "* %<%H:%M> - %?\n%U\n\n%i")
          ("s" "Steal"
           entry
           (file+olp+datetree "journal.org")
           "* %<%H:%M> - %?\n%U\n\n#+begin_quote\n%i\n#+end_quote\n\n%a\n"))))

;; Org lists - Replace ~-~ by ~•~ on unordered lists.
(font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Org Habits - tracking habits
(use-package org-habit
  :ensure nil
  :after org
  :config
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 79
        org-habit-completed-glyph ?.
        org-habit-following-days 1
        org-habit-show-habits nil
        org-habit-show-all-today t))

;; Org Agenda - setup and custom views
;; Custom agenda views, agenda settings, and so on.
(use-package org-agenda
  :ensure nil
  :config

  (defun my/org-agenda-show-all-dates ()
    (interactive)
    (setq org-agenda-show-all-dates (not org-agenda-show-all-dates))
    (org-agenda-redo))

  (defun my/org-agenda-breadcrumb ()
    (if (= (org-outline-level) 3)
        " ↳ "
      ""))

  (defun my/org-project-p ()
    (and (= (org-outline-level) 2)
         (string-match "\\[[0-9]*[/%][0-9]*\\]"
                       (org-entry-get nil "ITEM"))))

  (defun my/org-todo-next-p ()
    (and (org-entry-is-todo-p)
         (not (org-get-scheduled-time nil))
         (not (my/org-project-p))))

  (defun my/org-project-next-p ()
    (and (my/org-project-p)
         (not (my/org-project-stuck-p))))

  (defun my/org-todo-index ()
    (let ((index -1)
          (level (org-outline-level))
          (found nil)
          (line (line-number-at-pos)))
      (save-excursion
        (org-up-element)
        (org-map-tree
         (lambda ()
           (when (and (not found)
                      (org-entry-is-todo-p)
                      (= (org-outline-level) level))
             (setq index (+ index 1))
             (setq found (= line (line-number-at-pos)))))))
      index))

  (defun my/org-next-p ()
    (if (my/org-project-p)
        (setq org-agenda-todo-keyword-format "VISH %s")
      (setq org-agenda-todo-keyword-format "%s"))

    (and (org-entry-is-todo-p)
         (not (org-get-scheduled-time nil))
         (or
          ;; level 2 TODO = projects or single step tasks
          (and (= (org-outline-level) 2)
               (not (my/org-project-stuck-p)))

          ;; first level 3 TODO = project next action
          (and (= (org-outline-level) 3)
               (= (my/org-todo-index) 0)))))

  (defun my/org-project-stuck-p ()
    (let ((free nil))
      (when (my/org-project-p)
        (org-map-tree
         (lambda ()
           (when (and (not free)
                      (= (org-outline-level) 3)
                      (org-entry-is-todo-p))
             (setq free t))))
        (not free))))

  (defun my/org-project-wip-p ()
    (and (my/org-project-p)
         (let ((wip nil))
           (org-map-tree
            (lambda ()
              (when (and (not wip)
                         (= (org-outline-level) 3)
                         (my/org-todo-wip-p))
                (setq wip t))))
           wip)))

  (defun my/org-todo-wip-p ()
    (when (and (org-entry-is-todo-p)
               (not (my/org-project-p)))

      (let* ((line (line-number-at-pos))
             (date (format-time-string "%Y-%m-%d"))
             (is-clocking (string-match
                           "CLOCK: \\[.*?\\]$"
                           (substring-no-properties (org-get-entry))))
             (clocked (string-match
                       "CLOCK: \\[.*?\\]--\\[.*?\\]"
                       (substring-no-properties (org-get-entry))))
             (clocked-today (string-match
                             (format "CLOCK: .*%s" date)
                             (substring-no-properties (org-get-entry)))))

        (if (org-get-repeat)
            ;; if its a repeating task, only consider it if it is being/was clocked today
            (or is-clocking clocked-today)

          ;; if its not a repeating task, its in progress if its being clocked or was ever clocked
          (or is-clocking clocked)))))

  (defun my/org-wip-p ()
    (or (my/org-project-wip-p)
        (my/org-todo-wip-p)))

  ;; (defun my/org-inbox-p ()
  ;;   (and (org-entry-is-todo-p)
  ;;        (not (org-get-scheduled-time nil))
  ;;        (not (my/org-wip-p))
  ;;        (not (my/org-project-p))
  ;;        (string= "D" (org-entry-get nil "PRIORITY"))))


  (setq org-scheduled-past-days 100
        org-agenda-start-with-log-mode nil
        org-agenda-window-setup 'current-window
        org-agenda-block-separator ?―
        org-agenda-start-day nil
        org-agenda-tags-column 'auto
        org-agenda-span 1
        org-agenda-show-all-dates nil
        org-agenda-skip-deadline-if-done t
        org-agenda-clockreport-parameter-plist '(:link t :maxlevel 2)
        org-agenda-skip-scheduled-if-done nil
        org-deadline-warning-days 3
        org-agenda-start-with-follow-mode nil
        org-agenda-compact-blocks nil
        org-agenda-use-time-grid t
        org-agenda-skip-archived-trees nil
        org-agenda-current-time-string "←"
        org-agenda-files '("tasks.org")
        org-agenda-log-mode-items '(closed state)
        org-agenda-scheduled-leaders '(" " "!")
        org-agenda-deadline-leaders '(" " "!")
        org-agenda-hide-tags-regexp "^\\(agenda\\|public\\|draft\\|drill\\|draft\\|birthdate\\)$"
        org-agenda-todo-keyword-format "%s"
        org-agenda-tag-filter-preset nil
        org-agenda-tag-filter nil
        org-agenda-prefix-format '((agenda . "  %-12t %s")
                                   (todo . "  %s")
                                   (tags . "  %(my/org-agenda-breadcrumb)")
                                   (search . "  %s"))
        org-agenda-time-grid
        '((daily today require-timed)
          (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200)
          " ┄┄┄┄┄ " ""))

  (setq org-agenda-custom-commands
        '(("a" "Agenda"
           ((agenda ""
                    ((org-agenda-span 'month)
                     (org-scheduled-past-days 100)
                     (org-deadline-warning-days 10)))))
          ("d" "To-do"
           ;; 3 day agenda
           ((agenda ""
                    ((org-deadline-warning-days 0)
                     (org-agenda-span 3)
                     (org-agenda-time-grid '((daily today require-timed)
                                             ()
                                             " ┄┄┄┄┄ " ""))
                     (org-agenda-show-all-dates t)))

            ;; Urgent
            ;; (tags-todo "+PRIORITY=\"A\""
            ;;            ((org-agenda-overriding-header "Urgent")))

            ;; In progress
            (tags-todo ".*"
                       ((org-agenda-overriding-header "In progress")
                        (org-agenda-skip-function
                         '(if (my/org-wip-p)
                              nil
                            (org-goto-sibling)
                            (point)))))

            ;; ;; Next actions
            (tags-todo ".*"
                       ((org-agenda-overriding-header "Next actions")
                        (org-agenda-skip-function
                         '(if (my/org-next-p)
                              nil
                            (org-goto-sibling)
                            (point)))))

            ;; Stuck projects
            (tags-todo ".*"
                       ((org-agenda-overriding-header "Stuck projects")
                        (org-agenda-skip-function
                         '(if (my/org-project-stuck-p)
                              nil
                            (org-goto-sibling)
                            (point)))))))

            ;; Inbox
            ;; (tags-todo "+TODO=\"TODO\"+LEVEL=2+PRIORITY=\"D\""
            ;;            ((org-agenda-overriding-header "Inbox")
            ;;             (org-agenda-skip-function
            ;;              '(if (my/org-inbox-p)
            ;;                   nil
            ;;                 (org-goto-sibling)
            ;;                 (point)))))

            ;; Someday
            ;; (tags-todo "+TODO=\"TODO\"+PRIORITY=\"E\""
            ;;            ((org-agenda-overriding-header "Someday")))))
          ("e" "Tasks by effort"
           ((tags-todo "+TODO=\"TODO\"+Effort>\"\""
                       ((org-agenda-overriding-header "Tasks by effort")
                        (org-agenda-sorting-strategy '(effort-up))
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled))
                        (org-agenda-prefix-format '((tags . "%-5e - ")))))))
          ("E" "Tasks without effort"
           ((tags-todo "+Effort=\"\""
                       ((org-agenda-overriding-header "Tasks without effort")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled))))))))

  (custom-set-faces
   '(org-agenda-current-time ((t (:foreground "green" :weight bold)))))

  :bind
  ((:map org-mode-map
         ("C-'" . nil))
   (:map global-map
         ("C-c a" . org-agenda)
         ("C-'" . (lambda ()
                    (interactive)
                    (require 'org-roam)
                    (find-file (expand-file-name "tasks.org" org-directory)))))
   (:map org-agenda-mode-map
         ("C-a" . my/org-agenda-show-all-dates)
         ("j" . org-agenda-next-line)
         ("C-j" . org-agenda-goto-date)
         ("h" . org-agenda-earlier)
         ("l" . org-agenda-later)
         ("C-d" . evil-scroll-down)
         ("C-w C-w" . evil-window-next)
         ("C-u" . evil-scroll-up)
         ("M-g" . org-agenda-toggle-time-grid)
         ("{" . org-agenda-backward-block)
         ("}" . org-agenda-forward-block)
         ("z" . evil-scroll-line-to-center)
         ("g" . evil-goto-first-line)
         ("G" . evil-goto-line)
         ("k" . org-agenda-previous-line))))

;; Show drawers folded by default
(use-package org
  :hook (org-mode . org-fold-hide-drawer-all))

;; format org after capture
(use-package org
  :hook (org-capture-before-finalize . my/org-format)
  :config
  (defun my/org-align-all-properties ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" nil t)
        (let ((drawer-start (point))
              drawer-end)
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
            (setq drawer-end (point))
            (goto-char drawer-start)
            (while (< (point) drawer-end)
              (when (looking-at "^[ \t]*:\\w+:")
                (org--align-node-property))
              (forward-line 1)))))))

  (defun my/org-format ()
    (interactive)
    (when (derived-mode-p 'org-mode)
      (org-align-all-tags)
      (org-update-statistics-cookies t)
      (my/org-align-all-properties))))

;; org-present - make presentations using org mode
;; (use-package org-present
;;   :hook ((org-present-mode
;;           . (lambda ()
;;               (org-present-hide-cursor)
;;               (setq display-line-numbers-type nil)
;;               (display-line-numbers-mode 1)))
;;          (org-present-mode-quit
;;           . (lambda ()
;;               (org-present-show-cursor)
;;               (setq display-line-numbers-type 'relative)
;;               (display-line-numbers-mode 1)))))

;; org-drill - same idea as Anki
(use-package org-drill
  :defer t
  :init
  (defun org-drill-agenda ()
    (interactive)
    (org-drill 'agenda))
  (defun org-drill-cram-agenda ()
    (interactive)
    (org-drill-cram 'agenda))
  (advice-add 'org-drill-time-to-inactive-org-timestamp :override
              (lambda (time)
                "Convert TIME into org-mode timestamp."
                (format-time-string
                 (concat "[" (cdr org-time-stamp-formats) "]")
                 time)))
  :config
  (add-to-list 'org-modules 'org-drill)
  (setq org-drill-add-random-noise-to-intervals-p t)
  ;;(setq org-drill-sm5-initial-interval 1.0)
  (setq org-drill-spaced-repetition-algorithm 'sm2))

;; toc-org - generate table of contents
;; Useful for github that doesn't create a TOC automatically
;; (use-package toc-org
;;   :hook
;;   (org-mode . toc-org-mode))

;; org-music - manage songs and playlists using org
(use-package org-music
  :after evil
  :vc
  (:url "https://github.com/debanjum/org-music.git")

  :preface
  (defun org-music-jump-to-current-song ()
    (interactive)
    (find-file org-music-file)
    (let* ((song-path (mpv-get-property "path"))
           (outline-name (when (string-match ".*/\\(.*\\)\\.m4a" song-path)
                           (match-string 1 song-path)))
           (outline-marker (org-find-exact-headline-in-buffer outline-name)))
      (when outline-marker
        (goto-char outline-marker))))

  (defun org-music-count-songs ()
    (interactive)
    (let ((count (apply '+ (org-map-entries (lambda ()
                       (if (string= "song" (org-entry-get (point) "TYPE"))
                           1
                         0))))))
      (message "You have %d songs in this buffer" count)))

  (defun org-music-goto-file ()
    (interactive)
    (find-file org-music-file))

  ;; (defun org-music-playlist-import (url)
  ;;   )
  ;; (shell-command-to-string "yt-dlp --flat-playlist --print '** \%(uploader)s - \%(title)s\n:PROPERTIES:\nTYPE: song\nQUERY: \%(uploader)s - \%(title)s\n:END:' 'https://music.youtube.com/playlist?list=PLRVUgK64Wzw0_zuHPme0GJgrqItat3oT2'")

  ;; (org-music-playlist-import)

  :init
  (setq
   org-music-file "~/Sync/Org/music.org"
   org-music-youtube-downloader "yt-dlp"
   org-music-media-directory "~/.cache/org-music"
   org-music-operating-system "linux"
   org-music-cache-size (* 10 1024)) ;; 10 GB?

  :bind
  (:map global-map
        ("C-c m c" . org-music-jump-to-current-song)
        ("C-c m F" . org-music-goto-file)
        ("C-c m l p" . org-music-play-list)
        ("C-c m l e" . org-music-enqueue-list)
        ("C-c m p p" . org-music-play-song-at-point)
        ("C-c m p e" . org-music-enqueue-song-at-point)))

;; org-indent-mode (builtin) - visually indent text inside headings
(use-package org
  :hook (org-mode . org-indent-mode))

;; org-appear - show emphasis markers when cursor is over the word.
(use-package org-appear
    :hook
    (org-mode . org-appear-mode)
    :config
    (setq org-hide-emphasis-markers t)
    (setq org-link-descriptive t)
    (setq org-pretty-entities nil)
    (setq org-hidden-keywords nil)
    (setq org-appear-autoemphasis t)
    (setq org-appear-autolinks t)
    (setq org-appear-autosubmarkers t)
    (setq org-appear-autoentities t)
    (setq org-appear-autokeywords t)
    (setq org-appear-inside-latex t))

;; org-fragtog - display LaTeX automatically inside org buffers
(use-package org-fragtog
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-startup-with-latex-preview t)
  :custom
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

;; org-roam - org knowledge management system
(use-package org-roam
  :init
  ;; add roam node files that contains specific tags to agenda
  (defun my/add-roam-files-to-agenda (&rest args)
    (require 'org-roam)
    (let ((files (org-roam-db-query "
select distinct n.file
from nodes n
join tags t on t.node_id = n.id
where t.tag in ('\"agenda\"', '\"drill\"')
limit 20
")))
      (setq org-agenda-files nil)
      (mapc (lambda (item)
              (add-to-list 'org-agenda-files (car item)))
            files)
      nil))

  (defun my/org-roam-node-sort (cmp-a cmp-b)
    (let ((a (cdr cmp-a))
          (b (cdr cmp-b)))
      (cond
       ((member "drill" (org-roam-node-tags a)) nil)
       ((member "agenda" (org-roam-node-tags a)) nil)
       ((member "journal" (org-roam-node-tags a)) nil)
       ((member "drill" (org-roam-node-tags b)) t)
       ((member "agenda" (org-roam-node-tags b)) t)
       ((member "journal" (org-roam-node-tags b)) t)
       (t (org-roam-node-read-sort-by-file-atime cmp-a cmp-b)))))

  (defun my/org-roam-node-find ()
    (interactive)
    (org-roam-node-visit (org-roam-node-read nil nil 'my/org-roam-node-sort)))

  (advice-add 'org-agenda :before #'my/add-roam-files-to-agenda)
  :config
  (when (not (file-directory-p "~/Sync/Org"))
    (make-directory "~/Sync/Org"))

  (setq org-roam-db-location "~/Sync/Org/org-roam.db")
  (setq org-roam-directory "~/Sync/Org")

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "${slug}.org" "#+title: ${title}")
           :unnarrowed t)
          ("P" "public" plain "%?" :target
           (file+head "${slug}.org" "* ${title} :public:")
           :unnarrowed t)
          ("p" "politics" plain "%?" :target
           (file+head "${slug}.org" "* ${title} :politics:")
           :unnarrowed t)))

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %(format-time-string \"%H:%M\") %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d> "))))

  (org-roam-db-autosync-enable)

  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n F" . my/org-roam-node-find)
   ("C-c n w" . org-roam-refile)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n c" . org-roam-dailies-capture-today)
   ("C-c n d" . org-roam-dailies-find-date)))

;; org-roam-ql - query language for org roam
(use-package org-roam-ql :defer)

;; maybe i should use org-roam-dailies here?
(defun my/org-roam-timeline (&optional query)
  (interactive)
  (let* ((query (or query '(funcall (lambda (node) t))))
         (nodes (org-roam-ql-nodes
                 `(and (title "^[0-9]\\{4\\}/[0-9]\\{2\\}/[0-9]\\{2\\}")
                       ,query)
                 "title")))
    (generate-new-buffer "*org-roam-timeline*")
    (switch-to-buffer "*org-roam-timeline*")
    (erase-buffer)
    (setq my/last-date nil)
    (mapc (lambda (node)
            (let* ((full-title (org-roam-node-title node))
                   (parts (string-split full-title ": "))
                   (date (car parts))
                   (title (car (cdr parts))))
              (if (and my/last-date (string= my/last-date date))
                  (insert (concat "- " title "\n"))
                (when my/last-date
                  (insert "\n"))
                (setq my/last-date date)
                (insert (concat date ":\n" "- " title "\n")))))
          nodes)))

;; org-roam-ui - visualize Org Roam graph in real time.
(use-package org-roam-ui :defer t)

;; org-cliplink - paste link with automatic title
(use-package org-cliplink :defer t)

;; org-download - getting images into org
(use-package org-download
  :defer t
  :config
  (org-download-enable)
  (setq org-download-screenshot-method "sleep 1; spectacle --region -o %s")
  :bind
  ("C-c o i s" . org-download-screenshot)
  ("C-c o i D" . org-download-delete)
  ("C-c o i R" . org-download-rename-at-point)
  ("C-c o i p" . org-download-clipboard))

;; proced (builtin)
(use-package proced
  :ensure nil
  :custom
  (proced-auto-update-flag t)
  (proced-auto-update-interval 2)
  (proced-enable-color-flag t)
  (proced-tree-flag t)
  :config
  ;; TODO: use advice
  (defun proced-format-args (s)
    (let* ((chunks (split-string s " "))
           (cmd-path (car chunks))
           (cmd-name (file-name-nondirectory cmd-path))
           (new-chunks (append (list cmd-name) (cdr chunks))))
      (mapconcat 'identity new-chunks " "))))

;; eshell (builtin)
(use-package esh-mode
  :ensure nil
  :after compile
  :init
  (defun my/eshell-search-history ()
    (interactive)
    (let* ((input (eshell-get-old-input))
           (hist (ring-elements eshell-history-ring))
           (cmd nil))
      (setq cmd
            (completing-read "command from history: " hist nil t input))
      (when cmd
        (eshell-kill-input)
        (insert cmd))))
  :hook ((eshell-mode . (lambda ()
                          (setq-local company-idle-delay nil)))
         (eshell-mode . compilation-shell-minor-mode)
         (eshell-pre-command . eshell-save-some-history))
  :custom
  (eshell-highlight-prompt t)
  (eshell-history-size 1000)
  (eshell-hist-ignoredups t)
  :bind
  (("C-x e" . eshell)
   :map eshell-mode-map
   ("C-a" . backward-sentence)
   ("C-e" . forward-sentence)
   ("C-r" . my/eshell-search-history)
   ("C-l" . (lambda ()
              (interactive)
              (let ((input (eshell-get-old-input)))
                (eshell-kill-input)
                (eshell/clear)
                (recenter-top-bottom 'top)
                (insert input))))
   ("C-c C-l" . (lambda ()
                  (interactive)
                  (let ((input (eshell-get-old-input)))
                    (eshell/clear t)
                    (eshell-send-input)
                    (insert input))))))

;; eshell: unbind C-c C-l
(use-package em-hist
  :ensure nil
  :after esh-mode
  :bind (:map eshell-hist-mode-map
              ("C-c C-l" . nil)
              ("C-c C-x" . nil)))

;; eshell: use TAB for company popup
(use-package em-cmpl
  :ensure nil
  :after (esh-mode company)
  :bind
  (:map eshell-cmpl-mode-map
   ("TAB" . company-complete)))

(use-package eshell-prompt-extras
  :after esh-mode
  :config
  (setq eshell-prompt-function 'epe-theme-lambda))

;; deno lsp
(use-package eglot
  :defer
  :config
  (add-to-list
   'eglot-server-programs
   '((js-mode typescript-mode
              (typescript-ts-base-mode :language-id "typescript"))
     . (eglot-deno "deno" "lsp")))

  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list :enable t :lint t)))
