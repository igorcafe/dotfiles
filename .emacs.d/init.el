;; reduce GC impact
(setq gc-cons-threshold (* 800000 10))

;; show GC messages
(setq garbage-collection-messages t)

;; setup straight.el
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

(straight-use-package 'use-package)

;; Always use straight unless specificied not to
(setq straight-use-package-by-default t)

;; Don't check for modifications on startup
;; use M-x straight-rebuild-package instead
(setq straight-check-for-modification 'never)

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

;; Use =ibuffer= (builtin) instead of list-buffers.
(use-package emacs
  :bind ("C-x C-b" . ibuffer))

;; Persist minibuffer's history
;; In ~M-x~, ~C-x C-f~ and so on.
(savehist-mode 1)
(setq history-length 100)

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
  :straight t
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
   :map evil-insert-state-map
   ("C-a" . nil)
   ("C-e" . nil))
  :config
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :straight t
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

;; all-the-icons + =all-the-icons-dired= - icon packages
(use-package all-the-icons
  :after doom-modeline)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

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

;; project.el (builtin) - managing projects
;; Helps you manage projects based on version control systems, like
;; git repos. Check =C-x p p=.
;; Launch vterm in the project's root directory.
;; (defun project-vterm ()
;;   (interactive)
;;   (let* ((proj-dir (car (last (project-current))))
;;          (proj-name (file-name-nondirectory
;;                      (directory-file-name proj-dir)))
;;          (chosen-name (read-string "buffer name: " proj-name))
;;          (default-directory proj-dir))
;;     (vterm (format "vterm - %s" chosen-name))))


;; custom function for launching eshell with descriptive buffer names
(defun my/project-eshell ()
  (interactive)
  (let* ((default-directory (project-root (project-current t)))
         (proj-name (file-name-nondirectory
                     (directory-file-name default-directory)))
         (buffer-name (read-string "buffer name: "
                                   (format "eshell - %s" proj-name)))
         (eshell-buffer (get-buffer buffer-name)))
    (if eshell-buffer
        (pop-to-buffer eshell-buffer)
      (with-current-buffer (eshell t)
        (rename-buffer buffer-name)))))


;; Customize project.el commands.
(use-package project
  :config
  (setq project-switch-commands
        '((project-find-file "Find file" ?f)
          (project-find-regexp "Find regexp" ?g)
          (project-find-dir "Find directory" ?d)
          (project-vterm "vterm" ?t)
          ;;(project-vc-dir "VC-Dir")
          (my/project-eshell "Eshell" ?e)
          ;;(project-any-command "Other")
          (magit-project-status "Magit" ?m)))
  :bind
  (:map project-prefix-map
        ("t" . project-vterm)
        ("e" . my/project-eshell)
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

;; vue-mode - Vue support
(use-package vue-mode :defer t)

;; c-mode (builtin) - C/C++ support
(use-package emacs
  :hook (c-mode . (lambda ()
                    (setq c-basic-offset 2)
                    (setq indent-tabs-mode nil))))

;; eglot (builtin) - LSP client
;; Eglot is a builtin LSP (Language Server Protocol) client for emacs.
(use-package eglot
  :after evil
  :hook
  ;; before saving, if eglot is enabled, try to format and organize imports
  ((before-save
    . (lambda ()
        (when (and (fboundp 'eglot-managed-p) (eglot-managed-p))
          (call-interactively 'eglot-format)
          (call-interactively 'eglot-code-action-organize-imports))))

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
    :config
    (eldoc-box-hover-at-point-mode 1)
    (advice-add 'eldoc-doc-buffer :override 'eldoc-box-help-at-point))

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
         ("TAB" . company-complete))))

;; envrc - direnv integration
;; Works better than =direnv-mode= for me.
(use-package envrc
  :defer 0.5
  :config
  (envrc-global-mode))

(defun auth-get-password (host login)
  (let* ((entry (nth 0 (auth-source-search
                        :host host
                        :login login
                        :require '(:secret))))
         (secret (plist-get entry :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

;; aider
(use-package aider
  :straight
  (:host
   github
   :repo "tninja/aider.el"
   :files ("aider.el"
           "aider-core.el"
           "aider-file.el"
           "aider-code-change.el"
           "aider-discussion.el"
           "aider-prompt-mode.el"))
  :config
  (setq aider-args
        '("--model" "gpt-4o"
          "--no-auto-commits"
          (format "--api-key openai=%s"
                  (auth-get-password "openai" "key"))))
  :bind
  ("C-c i" . aider-transient-menu))

;; recentf-mode (builtin) - persistent history of recent files
;; Show recent files with ~C-x C-r~.
(use-package recentf
  :straight nil
  :config
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 100)
  (recentf-mode 1)
  :bind ("C-x C-r" . recentf-open))

;; use-package org to avoid any bugs
(use-package org :defer t)

;; olivetti - horizontal paddings for windows
(use-package olivetti
  :hook ((prog-mode
          go-dot-mod-mode
          eww-mode
          text-mode
          conf-mode
          org-agenda-mode
          restclient-mode)
         . olivetti-mode)
  :init
  (setq-default olivetti-body-width 100))

;; save-place-mode - save cursor position per file
(save-place-mode 1)

;; consult - multiple search utilities
(use-package consult
  :after evil
  :bind
  (:map evil-normal-state-map
        ;; analogous to project-find-regexp
        ("SPC p g" . consult-git-grep)

        ;; analogous to project-find-file
        ("SPC p f" . consult-project-buffer)

        ;; buffer errors
        ("SPC b e" . consult-flymake)

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
(use-package emacs
  :config
  (setq tab-bar-show nil)
  :bind (("M-1" . tab-select)
         ("M-2" . tab-select)))

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
  :straight nil
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
  :bind
  ("C-x g" . magit))

;; diff-hl - highlight uncommited changes
(use-package diff-hl
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (after-save . diff-hl-update))
  :config
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
;;   :straight nil
;;   :after evil
;;   :bind
;;   ((:map evil-normal-state-map
;;          (("SPC t" . my/vterm)))
;;    (:map vterm-mode-map
;;          (("M-1" . nil)
;;           ("M-2" . nil)))))


;; terminal emulator
(use-package eat
  :hook (eshell-mode . eat-eshell-mode))

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
        (lambda ()
          (let* ((path (emms-track-description
                        (emms-playlist-current-selected-track)))
                 (song (when (string-match ".*? - \\(.*\\)\\.m4a$" path)
                         (match-string 1 path))))
            (format "🎵 %s  " song))))
  :bind
  (:map global-map
        ("C-c m j" . mpv-playlist-next)
        ("C-c m k" . mpv-playlist-prev)
        ("C-c m ," . mpv-seek-backward)
        ("C-c m ." . mpv-seek-forward)
        ("C-c m SPC" . mpv-pause)
        ("C-c m s" . mpv-kill)
        ("C-c m e" . mpv-jump-to-playlist-entry)))

;; telega - telegram client
(use-package telega
  :straight nil ;; installed and built through nix
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
  :straight nil
  :defer t
  :config
  (setq auth-sources '("~/.authinfo.gpg")))

;; pinentry - for entrying pin for gpg
(use-package pinentry
  :defer 2
  :custom
  (epg-pinentry-mode 'loopback)
  :config
  (pinentry-start))

;; gnus - email client, news reader, maybe
(use-package gnus
  :straight nil
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
  (setq eww-auto-rename-buffer
        (lambda ()
          (let ((domain
                 (url-host
                         (url-generic-parse-url (plist-get eww-data :url))))
                (title (plist-get eww-data :title)))
            (format "%s - %s # eww"
                    (truncate-string-to-width domain 20 nil nil "...")
                    (truncate-string-to-width title 30 nil nil "..."))))))

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

  :straight
  (:type git :host nil :repo "https://git.thanosapollo.org/yeetube")

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
  :straight nil
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
(setq org-priority-lowest ?D)
(setq org-priority-default ?D)
(setq org-priority-faces
      '((?A . (:foreground "gray"))
        (?B . (:foreground "gray"))
        (?C . (:foreground "gray"))
        (?D . (:foreground "gray"))))

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
        '((sequence "TODO" "|" "DONE")))
  (set-face-attribute 'org-done nil)
  (set-face-attribute 'org-headline-done nil :strike-through t :foreground "gray")
  :bind
  ;; the keybindings are the same, just made them global
  (("C-c C-x C-o" . org-clock-out)
   ("C-c C-x C-j" . org-clock-goto)))

;; Org Clock - keybindings
(use-package org
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
        '(("c"
           "Capture to inbox"
           entry
           (file+headline "tasks.org" "Tasks")
           "* INBX %?\n%U")
          ("j" "Journal"
           entry
           (file+headline "journal.org" "Journal")
           "* %T - %?"))))

;; Org lists - Replace ~-~ by ~•~ on unordered lists.
(font-lock-add-keywords 'org-mode
    '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Org Agenda - setup and custom views
;; Custom agenda views, agenda settings, and so on.
(defun my/org-agenda-show-all-dates ()
  (interactive)
  (setq org-agenda-show-all-dates
        (if org-agenda-show-all-dates nil t))
  (org-agenda-redo))

(defun my/org-agenda-breadcrumb ()
  (let ((parent (cdr (org-get-outline-path))))
    (if parent
        (format "[%s] " (mapconcat 'identity parent " > "))
      "")))


(use-package org-agenda
  :straight nil
  :init
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
        org-stuck-projects '("TODO=\"PROJ\"" ("NEXT" "WAIT") nil "")
        org-agenda-scheduled-leaders '(" " "!")
        org-agenda-deadline-leaders '(" " "!")

        org-agenda-todo-keyword-format "%s"
        org-agenda-prefix-format '((agenda . "  %-12t %s %(my/org-agenda-breadcrumb)")
                                   (todo . "  %(my/org-agenda-breadcrumb)")
                                   (tags . "  %(my/org-agenda-breadcrumb)")
                                   (search . "  %(my/org-agenda-breadcrumb)"))

        org-agenda-time-grid
        '((daily today require-timed)
          (800 900 1000 1100 1200 1300 1400 1500 1600 1700 1800 1900 2000 2100 2200)
          " ┄┄┄┄┄ " "")

        org-agenda-custom-commands
        '(("p" "Projects"
           ((todo "PROJ"
                      ((org-agenda-overriding-header "Projects")))
           ))
          ("a" "Agenda"
           ((agenda ""
                    ((org-agenda-span 10)
                     (org-scheduled-past-days 100)
                     (org-deadline-warning-days 10)))))
          ("d" "To-do"
           ((agenda ""
                    ((org-agenda-span 3)
                     (org-agenda-time-grid '((daily today require-timed)
                        ()
                        " ┄┄┄┄┄ " ""))))
            (tags-todo "+PRIORITY=\"A\""
                       ((org-agenda-overriding-header "Urgent")))
            (todo "NEXT"
                       ((org-agenda-overriding-header "In progress")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'notregexp "CLOCK: \\[." 'scheduled))))
            (todo "WAIT"
                  ((org-agenda-overriding-header "Waiting")
                   (org-agenda-sorting-strategy '(alpha-up))))
            (tags-todo "+TODO=\"NEXT\""
                       ((org-agenda-overriding-header "Next actions")
                        (org-agenda-sorting-strategy '(alpha-up))
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if
                           'regexp "CLOCK: \\[."
                           'scheduled))))
            ;; (tags-todo "+TODO=\"NEXT\"+LEVEL=3"
            ;;            ((org-agenda-overriding-header "Project next tasks")
            ;;             (org-agenda-sorting-strategy '(alpha-up))
            ;;             (org-agenda-skip-function
            ;;              '(org-agenda-skip-entry-if
            ;;                'regexp "CLOCK: \\[."
            ;;                'scheduled))))
            (todo "PROJ"
                  ((org-agenda-overriding-header "Projects")))
            (todo "INBX"
                       ((org-agenda-overriding-header "Inbox")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if
                           'regexp "CLOCK: \\[."
                           'scheduled 'done))))
            (todo "SMDY"
                       ((org-agenda-overriding-header "Someday")
                        (org-agenda-skip-function
                         '(org-agenda-skip-entry-if 'scheduled 'done))))))
          ("w" "Agenda"
           ((agenda ""
                    ((org-agenda-files '("work.org"))
                     (org-agenda-span 100)
                     (org-scheduled-past-days 0)
                     (org-deadline-warning-days 0)))))
          ("e" "Tasks by effort"
           ((tags-todo "-TODO=\"DONE\"-TODO=\"FINI\"+Effort>\"\""
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
  ((:map global-map
         ("C-c a" . org-agenda)
         ("C-'" . org-cycle-agenda-files))
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
  (advice-add 'org-drill-time-to-inactive-org-timestamp :override
              (lambda (time)
                "Convert TIME into org-mode timestamp."
                (format-time-string
                 (concat "[" (cdr org-time-stamp-formats) "]")
                 time)))
  :config
  (add-to-list 'org-modules 'org-drill))

;; toc-org - generate table of contents
;; Useful for github that doesn't create a TOC automatically
(use-package toc-org
  :hook
  (org-mode . toc-org-mode))

;; org-music - manage songs and playlists using org
(use-package org-music
  :after evil

  :straight
  (:host github :repo "debanjum/org-music" :branch "master")

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
        ("C-c m R" . mpv-playlist-shuffle)
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
    (setq org-pretty-entities t)
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
  :config
  (when (not (file-directory-p "~/Sync/Org/Roam"))
    (make-directory "~/Sync/Org/Roam"))
  (setq org-roam-directory "~/Sync/Org/Roam")

  (setq org-roam-capture-templates
        '(("d" "default" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "* ${title}")
           :unnarrowed t)
          ("p" "politics" plain "%?" :target
           (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "* ${title} :politics:")
           :unnarrowed t)))

  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %(format-time-string \"%H:%M\") %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d> "))))

  (org-roam-db-autosync-enable)

  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n c" . org-roam-dailies-capture-today)
   ("C-c n d" . org-roam-dailies-find-date)))

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
  :straight nil
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
  :straight nil
  :hook ((eshell-mode . (lambda ()
                          (setq-local company-idle-delay nil)))
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
   ("C-l" . (lambda ()
              (interactive)
              (eshell/clear)
              (recenter-top-bottom 'top)))
   ("C-c C-l" . (lambda ()
                  (interactive)
                  (let ((input (eshell-get-old-input)))
                    (eshell/clear t)
                    (eshell-send-input)
                    (insert input))))))

(use-package eshell-prompt-extras
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
