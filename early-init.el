;;; early-init.el --- Early initialization -*- lexical-binding: t -*-
;; Loaded before the package system and GUI is initialized.

;; Disable package.el — we use Elpaca instead.
(setq package-enable-at-startup nil)

;; Raise GC threshold during startup to reduce GC pauses.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reset GC after startup to a sane value.
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

;; Disable UI chrome as early as possible to avoid flashing.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Prevent frame resize on font/UI changes during startup.
(setq frame-inhibit-implied-resize t)

;; Suppress compiler warnings and "ad-handle-definition" noise.
(setq native-comp-async-report-warnings-errors nil
      byte-compile-warnings '(not obsolete))

;;; early-init.el ends here
