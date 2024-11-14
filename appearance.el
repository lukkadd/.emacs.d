(use-package dashboard

    :config
    (setq dashboard-banner-logo-title "Anvil Emacs")
    (setq dashboard-startup-banner 'logo)
    (setq dashboard-center-content t)
    (setq dashboard-vertically-center-content t)
    (setq dashboard-items '((recents   . 5)
                            (bookmarks . 5)
                            (projects  . 5)
                            (registers . 5)))
    (setq dashboard-startup-banner (concat (expand-file-name user-emacs-directory) "assets/anvil.png"))
    (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(setq inhibit-startup-message t)
  (scroll-bar-mode -1)        ; Disable visible scrollbar
  (tool-bar-mode -1)          ; Disable the toolbar
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10)        ; Give some breathing room
  (menu-bar-mode -1)            ; Disable the menu bar

  ;; Set up the visible bell
  (setq visible-bell t)

  ;; add line numbers (relative)
  (column-number-mode)
  (global-display-line-numbers-mode t)
  (setq display-line-numbers-type 'relative)

  ;; Set frame transparency (Since I'm using 100% transparency it's effectively useless)
  (set-frame-parameter (selected-frame) 'alpha lukkadd/frame-transparency)
  (add-to-list 'default-frame-alist `(alpha . ,lukkadd/frame-transparency))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Disable line numbers for some modes
  (dolist (mode '(term-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set emacs UI font
(set-face-attribute 'default t :font lukkadd/fixed-pitch-font :height lukkadd/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch t :font lukkadd/fixed-pitch-font :height lukkadd/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch t :font lukkadd/variable-pitch-font :height lukkadd/default-variable-font-size :weight 'regular)

;; For new frames
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (set-face-attribute 'default nil :font lukkadd/fixed-pitch-font :height lukkadd/default-font-size)
              (set-face-attribute 'fixed-pitch frame :font lukkadd/fixed-pitch-font :height lukkadd/default-font-size)
              (set-face-attribute 'variable-pitch frame :font lukkadd/variable-pitch-font :height lukkadd/default-variable-font-size :weight 'regular))))

(use-package kanagawa-themes
  :init (load-theme 'kanagawa-dragon t))

(use-package doom-themes)

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package hl-todo
  :init
  (global-hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FAE675")
          ("FIXME"  . "#F23847")
          ("DEBUG"  . "#ACB7FF")
          ("STUB"   . "#A0FFA0")))
  )

;; TODO: Bind todo related keymaps

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
