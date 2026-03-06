;;; init.el --- Main configuration -*- lexical-binding: t -*-

;;; ── Elpaca Package Manager Bootstrap ─────────────────────────────────────────

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; use-package integration — install everything via Elpaca by default.
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

;;; ── Core Settings ─────────────────────────────────────────────────────────────

(use-package emacs
  :ensure nil
  :init
  ;; Startup
  (setq inhibit-startup-message t
        initial-scratch-message nil)

  ;; Behaviour
  (setq ring-bell-function 'ignore
        use-short-answers t         ; "y" instead of "yes"
        confirm-kill-emacs nil)

  ;; Scrolling
  (setq scroll-conservatively 101
        scroll-margin 4)

  ;; Indentation — no tabs, 4-space default
  (setq-default indent-tabs-mode t
                tab-width 4)

  ;; Keep backup and auto-save files out of the way
  (setq backup-directory-alist
        `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
        auto-save-file-name-transforms
        `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t))
        create-lockfiles nil)

  ;; Relative line numbers everywhere (except terminals / help)
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers-type 'relative)
  (dolist (hook '(term-mode-hook shell-mode-hook eshell-mode-hook
                  help-mode-hook))
    (add-hook hook (lambda () (display-line-numbers-mode -1))))

  ;; Font — change family/height to match your setup.
  ;; Requires a Nerd Font for icons (doom-modeline).
  ;; Recommended: "JetBrainsMono Nerd Font" — install via your package manager.
  (set-face-attribute 'default nil
                      :family "JetBrainsMono Nerd Font"
                      :height 120)

  ;; Misc UI
  (global-hl-line-mode 1)
  (column-number-mode 1))

;;; ── Evil Mode ─────────────────────────────────────────────────────────────────

(use-package evil
  :demand t
  :init
  ;; These must be set BEFORE evil loads.
  (setq evil-want-integration t
        evil-want-keybinding nil   ; handed off to evil-collection
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

;; Wait for evil before loading evil-collection.
(elpaca-wait)

(use-package evil-collection
  :demand t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-commentary
  :demand t
  :after evil
  :config
  ;; gc{motion} to comment, gcc to comment line — same as vim-commentary.
  (evil-commentary-mode 1))

;;; ── Theme ─────────────────────────────────────────────────────────────────────

(use-package kanagawa-themes
  :demand t
  :config
  (load-theme 'kanagawa-dragon t))

;;; ── Modeline ──────────────────────────────────────────────────────────────────

;; nerd-icons: after installing, run M-x nerd-icons-install-fonts once.
(use-package nerd-icons
  :demand t)

(use-package doom-modeline
  :demand t
  :after nerd-icons
  :init
  (setq doom-modeline-height 32
        doom-modeline-bar-width 4
        doom-modeline-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project)
  :config
  (doom-modeline-mode 1))

;;; ── Which-key ─────────────────────────────────────────────────────────────────

(use-package which-key
  :demand t
  :init
  (setq which-key-idle-delay 0.4)
  :config
  (which-key-mode 1))

;;; ── Keybinding Framework ──────────────────────────────────────────────────────

(use-package general
  :demand t
  :config
  (general-evil-setup t)
  ;; Define the SPC leader for normal, visual, and motion states.
  (general-create-definer my/leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"))

(elpaca-wait)

;;; ── Navigation ────────────────────────────────────────────────────────────────

(use-package avy
  :demand t)

;;; ── Completion UI ─────────────────────────────────────────────────────────────

(use-package vertico
  :demand t
  :init
  (setq vertico-cycle t       ; wrap around at top/bottom
        vertico-count 15)
  :config
  (vertico-mode 1))

(use-package orderless
  :demand t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; Allow tramp/remote paths to still work correctly.
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :demand t
  :config
  (marginalia-mode 1))

(use-package consult
  :demand t
  :bind
  (;; Buffer / file navigation
   ("C-x b"   . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ;; Search
   ("M-s l"   . consult-line)
   ("M-s g"   . consult-grep)
   ("M-s r"   . consult-ripgrep)
   ;; Jump
   ("M-g g"   . consult-goto-line)
   ("M-g i"   . consult-imenu)))

(use-package embark
  :demand t
  :bind
  (("C-."   . embark-act)       ; context actions on candidate at point
   ("C-;"   . embark-dwim)      ; default action
   ("C-h B" . embark-bindings)) ; alternative which-key
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :demand t
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;; ── In-buffer Completion ──────────────────────────────────────────────────────

(use-package corfu
  :demand t
  :init
  (setq corfu-auto t           ; show popup automatically
        corfu-auto-delay 0.2   ; seconds before popup appears
        corfu-auto-prefix 2    ; minimum chars before triggering
        corfu-cycle t          ; wrap around candidates
        corfu-preselect 'prompt
        corfu-popupinfo-delay '(0.4 . 0.2))
  :bind
  (:map corfu-map
        ("TAB"     . corfu-next)
        ([tab]     . corfu-next)
        ("S-TAB"   . corfu-previous)
        ([backtab] . corfu-previous)
        ("RET"     . corfu-insert)
        ("C-g"     . corfu-quit))
  :config
  (global-corfu-mode 1)
  (corfu-history-mode 1)      ; remember recently used candidates
  (corfu-popupinfo-mode 1))   ; show documentation popup alongside

(use-package nerd-icons-corfu
  :demand t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :demand t
  :config
  ;; These are fallback sources when LSP / mode-specific capfs don't apply.
  ;; cape-file first so file paths complete anywhere.
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; dabbrev last — fuzzy matches from open buffers.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

;;; ── LSP (Eglot) ───────────────────────────────────────────────────────────────

(use-package eglot
  :ensure nil   ; built-in since Emacs 29
  :hook
  ;; These cover both classic and treesitter modes (Step 5 will add ts modes).
  ((c-mode c-ts-mode
    c++-mode c++-ts-mode
    go-mode go-ts-mode
    js-mode js-ts-mode
    typescript-mode typescript-ts-mode tsx-ts-mode
    lua-mode) . eglot-ensure)
  :init
  (setq eglot-autoshutdown t        ; shut down server when no buffers use it
        eglot-sync-connect 1        ; don't block startup, connect async
        eglot-connect-timeout 10)
  :config
  ;; Orderless works with eglot completions.
  (add-to-list 'completion-category-overrides '(eglot (styles orderless)))
  (add-to-list 'completion-category-overrides '(eglot-capf (styles orderless)))

  ;; Odin — ols (Odin Language Server) must be on $PATH.
  (add-to-list 'eglot-server-programs '(odin-mode . ("ols"))))

(use-package odin-mode
  :ensure (:host github :repo "mattt-b/odin-mode")
  :hook (odin-mode . eglot-ensure))

;; Better Corfu + Eglot integration: merge eglot and cape capfs into one
;; so they cooperate rather than compete.
(defun my/eglot-capf-setup ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'cape-file)
                    #'cape-dabbrev)))
(add-hook 'eglot-managed-mode-hook #'my/eglot-capf-setup)

;; consult-eglot: M-g s to search workspace symbols.
(use-package consult-eglot
  :demand t
  :after (consult eglot)
  :bind (:map eglot-mode-map
              ("M-g s" . consult-eglot-symbols)))

;;; ── Treesitter ────────────────────────────────────────────────────────────────

(use-package emacs
  :ensure nil
  :init
  ;; Level 4 = maximum syntax highlighting detail from treesitter.
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :demand t
  :init
  ;; 'prompt = ask before downloading and compiling a grammar.
  ;; Change to t to install silently, or nil to never auto-install.
  (setq treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; ── Magit & Git UI ────────────────────────────────────────────────────────────

;; Magit requires transient >= 0.12, newer than the Emacs 30 built-in.
(use-package transient
  :demand t)

(elpaca-wait)

(use-package magit
  :bind ("C-x g" . magit-status)
  :init
  ;; Open magit status in the same window instead of splitting.
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))

;; Show added/changed/deleted lines in the fringe, synced with Magit.
(use-package diff-hl
  :demand t
  :hook ((magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (global-diff-hl-mode 1))

;;; ── Formatters ────────────────────────────────────────────────────────────────

;; Apheleia formats asynchronously and doesn't move point.
;; Required binaries (install separately):
;;   JS/TS/Lua : prettier  (npm i -g prettier)
;;   Go        : goimports (go install golang.org/x/tools/cmd/goimports@latest)
;;   C         : clang-format (pacman -S clang)
;;   Odin      : odinfmt (ships with the Odin compiler)
(use-package apheleia
  :demand t
  :config
  ;; goimports is a superset of gofmt — it also manages import blocks.
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'goimports)
  (setf (alist-get 'go-mode    apheleia-mode-alist) 'goimports)
  (apheleia-global-mode 1))

;;; ── Language: Lua ─────────────────────────────────────────────────────────────

;; lua-ts-mode is built-in (Emacs 30). lua-mode is needed as the
;; non-treesitter fallback that treesit-auto falls back to.
(use-package lua-mode
  :hook (lua-mode . eglot-ensure))

;;; ── Language: Go ──────────────────────────────────────────────────────────────

;; go-ts-mode is built-in. Go canonical style mandates tabs.
(add-hook 'go-ts-mode-hook
          (lambda ()
            (setq-local tab-width 4
                        indent-tabs-mode t)))

;;; ── Language: C ───────────────────────────────────────────────────────────────

;; c-ts-mode is built-in. Formatting is handled by apheleia + clang-format.
(add-hook 'c-ts-mode-hook
          (lambda ()
            (setq-local c-ts-mode-indent-offset 4)))

;;; ── Project Management ────────────────────────────────────────────────────────

(use-package projectile
  :demand t
  :init
  (setq projectile-completion-system 'default  ; let Vertico handle UI
        projectile-switch-project-action #'projectile-find-file
        projectile-sort-order 'recentf          ; recent files first
        projectile-enable-caching t)
  :config
  (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package consult-projectile
  :demand t
  :after (consult projectile)
  :config
  ;; Override default projectile bindings with consult-enhanced versions
  ;; inside the existing C-c p prefix map.
  (define-key projectile-command-map (kbd "f") #'consult-projectile-find-file)
  (define-key projectile-command-map (kbd "p") #'consult-projectile-switch-project)
  (define-key projectile-command-map (kbd "b") #'consult-projectile-switch-to-buffer))

;;; ── UI Polish ─────────────────────────────────────────────────────────────────

;; Built-ins first.
(use-package emacs
  :ensure nil
  :init
  ;; Smooth pixel-level scrolling (Emacs 29+).
  (pixel-scroll-precision-mode 1)
  ;; Highlight matching bracket/paren instantly.
  (show-paren-mode 1)
  (setq show-paren-delay 0
        show-paren-style 'mixed))  ; highlight expression if off-screen

;; Startup dashboard.
(use-package dashboard
  :demand t
  :init
  (setq dashboard-banner-logo-title "Welcome back."
        dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-items '((recents   . 7)
                          (projects  . 5)
                          (bookmarks . 3)))
  :config
  (dashboard-setup-startup-hook))

;; Indent guides — uses treesitter for accurate placement.
(use-package indent-bars
  :ensure (:host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :init
  (setq indent-bars-treesit-support t
        indent-bars-width-frac 0.2
        indent-bars-pad-frac 0.1
        indent-bars-color-by-depth-cycle-length 4))

;; Colorize nested delimiters by depth.
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO / FIXME / NOTE / HACK in source code.
(use-package hl-todo
  :demand t
  :config
  (global-hl-todo-mode 1))

;;; ── User Keybindings ──────────────────────────────────────────────────────────

(load (expand-file-name "keybinds.el" user-emacs-directory))

;;; init.el ends here
