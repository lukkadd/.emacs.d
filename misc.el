;; set directory for backup files
  (setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/"
                                                           user-emacs-directory))))
  ;; auto-save-mode doesn't create the path automatically
  (make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

  (setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
        auto-save-filename-transforms `((".*" ,(expand-file-name "tmp/auto-saves" user-emacs-directory) t)))

;; move projectile, lsp and session files
(setq projectile-known-projects-file (expand-file-name "tmp/projectile-bookmarks.eld" user-emacs-directory)
      lsp-session-file (expand-file-name "tmp/.lsp-session-v1" user-emacs-directory))

        ;; NOTE: If you want to move everything out of the ~/.emacs.d folder
        ;; reliably, set `user-emacs-directory` before loading no-littering!
        ;(setq user-emacs-directory "~/.cache/emacs")

        (use-package no-littering)

    (let ((dir (no-littering-expand-var-file-name "lock-files/")))
        (make-directory dir t)
        (setq lock-file-name-transforms `((".*" ,dir t))))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
