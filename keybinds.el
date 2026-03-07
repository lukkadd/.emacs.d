;;; keybinds.el --- Evil leader keybindings -*- lexical-binding: t -*-
;; All SPC-prefixed bindings live here.
;; general.el and all packages must already be loaded before this file runs.

;;; ── Avy Jump ──────────────────────────────────────────────────────────────────

(general-define-key
  :states '(normal visual motion)
  "s" '(avy-goto-char-2        :which-key "jump to char")
  "S" '(avy-goto-line          :which-key "jump to line"))

(general-define-key
  :states '(normal visual motion)
  :keymaps 'override
  "K" '(eldoc-box-help-at-point :which-key "hover docs"))

;;; ── Buffers ───────────────────────────────────────────────────────────────────

(my/leader
  ","   '(consult-buffer      :which-key "switch buffer")
  "b"   '(:ignore t           :which-key "buffers")
  "b k" '(kill-current-buffer :which-key "kill buffer")
  "b n" '(next-buffer         :which-key "next buffer")
  "b p" '(previous-buffer     :which-key "prev buffer")
  "b r" '(revert-buffer       :which-key "revert buffer"))

;;; ── Files ─────────────────────────────────────────────────────────────────────

(my/leader
  "f"   '(:ignore t           :which-key "files")
  "f f" '(find-file           :which-key "find file")
  "f r" '(consult-recent-file :which-key "recent files")
  "f p" `(,(lambda () (interactive) (find-file user-emacs-directory))
          :which-key "config dir"))

;;; ── Git ───────────────────────────────────────────────────────────────────────

(my/leader
  "g"   '(:ignore t              :which-key "git")
  "g g" '(magit-status           :which-key "status")
  "g b" '(magit-blame            :which-key "blame")
  "g n" '(diff-hl-next-hunk      :which-key "next hunk")
  "g p" '(diff-hl-previous-hunk  :which-key "prev hunk")
  "g r" '(diff-hl-revert-hunk    :which-key "revert hunk"))

;;; ── Code / LSP ────────────────────────────────────────────────────────────────

(my/leader
  "c"   '(:ignore t                      :which-key "code")
  "c a" '(eglot-code-actions             :which-key "code actions")
  "c r" '(eglot-rename                   :which-key "rename symbol")
  "c f" '(apheleia-format-buffer         :which-key "format buffer")
  "c d" '(xref-find-definitions          :which-key "go to definition")
  "c D" '(xref-find-references           :which-key "find references")
  "c i" '(eglot-find-implementation      :which-key "go to implementation")
  "c x" '(flymake-show-buffer-diagnostics :which-key "diagnostics")
  "c k" '(eldoc-doc-buffer               :which-key "hover docs"))

;;; ── Search ────────────────────────────────────────────────────────────────────

(my/leader
  "s"   '(:ignore t        :which-key "search")
  "s s" '(consult-line     :which-key "search buffer")
  "s p" '(consult-ripgrep  :which-key "search project")
  "s i" '(consult-imenu    :which-key "jump to symbol"))

;;; ── Windows ───────────────────────────────────────────────────────────────────

(my/leader
  "w"   '(:ignore t          :which-key "windows")
  "w v" '(split-window-right :which-key "split right")
  "w s" '(split-window-below :which-key "split below")
  "w c" '(delete-window      :which-key "close window")
  "w w" '(other-window       :which-key "other window")
  "w h" '(windmove-left      :which-key "window left")
  "w j" '(windmove-down      :which-key "window down")
  "w k" '(windmove-up        :which-key "window up")
  "w l" '(windmove-right     :which-key "window right"))

;;; ── Project ───────────────────────────────────────────────────────────────────

(my/leader
  "p"   '(:ignore t                          :which-key "project")
  "p p" '(consult-projectile-switch-project  :which-key "switch project")
  "p f" '(consult-projectile-find-file       :which-key "find file")
  "p b" '(consult-projectile-switch-to-buffer :which-key "project buffer")
  "p s" '(consult-ripgrep                    :which-key "search project")
  "p k" '(projectile-kill-buffers            :which-key "kill buffers")
  "p i" '(projectile-invalidate-cache        :which-key "invalidate cache"))

;;; ── Help ──────────────────────────────────────────────────────────────────────

(my/leader
  "h"   '(:ignore t          :which-key "help")
  "h k" '(describe-key       :which-key "describe key")
  "h f" '(describe-function  :which-key "describe function")
  "h v" '(describe-variable  :which-key "describe variable")
  "h m" '(describe-mode      :which-key "describe mode"))

;;; ── Open ──────────────────────────────────────────────────────────────────────

(my/leader
  "o"   '(:ignore t :which-key "open")
  "o t" '(eshell    :which-key "terminal"))

;;; ── Quit ──────────────────────────────────────────────────────────────────────

(my/leader
  "q"   '(:ignore t                  :which-key "quit")
  "q q" '(save-buffers-kill-emacs    :which-key "quit emacs")
  "q r" '(restart-emacs              :which-key "restart emacs"))

;;; keybinds.el ends here
