(defun hexl-view-bits (start end)
  (interactive "r")
  (let ((result ""))

    (dolist (char (string-to-list (buffer-substring start end)))
      (if (char-equal char ?\s ) nil (setq result (concat result " " (int-to-binary-string (string-to-number (char-to-string char) 16))))))

    (message result)))

(defun int-to-binary-string (value)
  "Convert an integer VALUE to a binary string."
  (let ((result ""))
    (while (> value 0)
      (setq result (concat (if (= (logand value 1) 1) "1" "0") result))
      (setq value (ash value -1)))
    (while (< (length result) 4)
      (setq result(concat "0" result)))
    result))
(int-to-binary-string 10)

;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; Set fonts that are available on your system
(defvar lukkadd/fixed-pitch-font "MartianMono NFM")
(defvar lukkadd/variable-pitch-font "Arial")

;; You will most likely need to adjust this font size for your system!
(defvar lukkadd/default-font-size 105)
(defvar lukkadd/default-variable-font-size 160)

;; Make frame transparency overridable
(defvar lukkadd/frame-transparency '(100 . 100))

;; Set folder paths
(defvar lukkadd/org-folder "G:/My Drive/Ark/Org")
(defvar lukkadd/projects-folder "C:/Users/lukka/Projects")

;; Default directory
(setq default-directory lukkadd/projects-folder)

(set-language-environment "UTF-8")

;; Allow emacs to use more memory to minimize garbage collections during startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Add hook to emacs startup that displays the startup time in *Messages*
(defun lukkadd/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'lukkadd/display-startup-time)

(load (expand-file-name "package-manager.el" user-emacs-directory))

(load (expand-file-name "keybindings.el" user-emacs-directory))

(load (expand-file-name "appearance.el" user-emacs-directory))

(load (expand-file-name "coding.el" user-emacs-directory))

(load (expand-file-name "languages.el" user-emacs-directory))

(load (expand-file-name "org.el" user-emacs-directory))

(load (expand-file-name "file-management.el" user-emacs-directory))

(load (expand-file-name "misc.el" user-emacs-directory))
