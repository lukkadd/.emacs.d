(defun custom-cpp-compile()
  "Set the compile command to use my custom build script"
  (interactive)
  (message (projectile-project-root))
  (cd (projectile-project-root))
  (compile "build.bat")
  )

(defun custom-cpp-run()
  "Run cpp project using my run.bat"
  (interactive)
  (cd (projectile-project-root))
  (let ((script-path "run.bat"))
    (compile script-path)
    ))

(general-define-key
 :keymaps 'c++-ts-mode-map
 "<f5>" 'custom-cpp-compile)

(general-define-key
 :keymaps 'c++-ts-mode-map
 "<f6>" 'custom-cpp-run)

(add-hook 'c++-ts-mode-hook #'lsp-deferred)

(use-package zig-mode)

(use-package gdscript-mode
    :vc (:fetcher github :repo "godotengine/emacs-gdscript-mode"))

(use-package php-mode
  )

(use-package go-mode)

(add-hook 'go-mode-hook 'lsp)
(add-hook 'go-mode-hook (lambda ()
                          (setq tab-width 4)
                          (setq indent-tabs-mode 1)))

(use-package yaml-mode)
