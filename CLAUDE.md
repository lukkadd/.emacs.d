# Emacs Config — Claude Notes

## Project Goal
Building a modern Emacs config for programming, step by step.

## Stack
- **Package manager**: Elpaca (async, use-package integration)
- **Keybindings**: Evil + evil-collection + evil-commentary
- **Theme**: kanagawa-themes (kanagawa-dragon variant)
- **Modeline**: doom-modeline + nerd-icons
- **Discovery**: which-key

## Target Languages
JavaScript, TypeScript, C, Go, Odin, Lua, Elisp

## Emacs Version
30.2 — Emacs 29+ built-ins (Eglot, Treesitter) are available and preferred.

## File Structure
```
~/.config/emacs/
├── early-init.el   # pre-GUI setup, GC tuning, disables package.el
├── init.el         # all config and package setup
└── keybinds.el     # all SPC leader keybindings (general.el)
```

## Build Roadmap
- [x] Step 1: Foundation — Elpaca, Evil, Kanagawa theme, doom-modeline, which-key
- [x] Step 2: Completion UI — Vertico, Orderless, Marginalia, Consult
- [x] Step 3: In-buffer completion — Corfu + Cape
- [x] Step 4: LSP — Eglot (built-in)
- [x] Step 5: Treesitter (built-in, Emacs 29+)
- [x] Step 6: Language-specific + Magit
- [x] Step 7: Project management — Projectile + consult-projectile
- [x] Step 8: UI polish — dashboard, indent-bars, rainbow-delimiters, smooth scroll, hl-todo

## Avy
- Bound directly to `s` / `S` in normal/visual/motion states — no `evil-define-motion` wrapper
- `evil-define-motion` caused "wrong type argument: listp" errors due to evil passing extra state

## Hover Docs (eldoc-box)
- `K` in normal/visual/motion shows hover docs in a floating child frame popup via `eldoc-box-help-at-point`
- `SPC c k` also opens hover docs via `eldoc-doc-buffer` (in a window, useful for pinning)
- **Must use `:keymaps 'override`** for the `K` binding — evil-collection's mode-specific maps override plain state bindings

## Keybinding Gotchas
- Global evil state bindings (no keymap) can be silently overridden by evil-collection mode maps
- Use `:keymaps 'override` for bindings that must win everywhere (like `K`, `s`, `S`)
- The SPC leader already uses `override` via `general-create-definer`

## Key Decisions
- No `package.el`, Elpaca only
- `evil-want-keybinding nil` required before evil loads (evil-collection takes over)
- `evil-undo-system 'undo-redo` uses Emacs 28+ native undo-redo
- `elpaca-wait` called after evil block to ensure ordering before evil-collection
- Nerd Font required for doom-modeline icons (JetBrainsMono Nerd Font recommended)
- After first install: run `M-x nerd-icons-install-fonts`
- Directories for backups/auto-saves must be created explicitly with `make-directory` — Emacs won't create them automatically

## User Preferences
- Relative line numbers
- No tabs, 4-space indent default
- Dark theme preferred
