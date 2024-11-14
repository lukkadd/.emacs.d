;; Initialize package sources
  (require 'package)

  (setq package-archives '(("melpa" . "https://melpa.org/packages/")
                           ("org" . "https://orgmode.org/elpa/")
                           ("elpa" . "https://elpa.gnu.org/packages/")))

  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))

    ;; Initialize use-package on non-Linux platforms
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  (unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; Make sure every package called with use-package is installed
  (require 'use-package)
  (setq use-package-always-ensure t)

(use-package auto-package-update
    :custom
    (auto-package-update-interval 7)
    (auto-package-update-hide-results t)
    :config
    (auto-package-update-maybe)
    (auto-package-update-at-time "09:00"))
