## 2026-03-24

### Changed
- Modernized Emacs config: moved `.emacs` → `.emacs.d/init.el`, replaced third-party packages with Emacs 30 built-ins and lighter alternatives
  - Ivy/Counsel/Smex → Vertico + Orderless + Marginalia + Consult + Embark
  - Company → Corfu + Cape
  - lsp-mode/Flycheck/Tide → Eglot + Flymake
  - Projectile → project.el (kept `C-c p` prefix)
  - git-gutter → diff-hl
- Enabled tree-sitter modes, which-key, completion-preview, editorconfig
- Added claude-code-ide.el (eat terminal backend) for running Claude Code in Emacs
- Installed tree-sitter grammars (10 languages), language servers (pyright, typescript-language-server, bash-language-server), ripgrep

### Decisions
- Skip gptel — Claude Code subscription doesn't include API credits, and Claude Code CLI is sufficient
- Use ABI-14 grammar versions — Emacs 30.2 doesn't support ABI 15 from tree-sitter 0.26.x yet
- Use eat over vterm — pure elisp, no compilation needed

### Open threads
- claude-code-ide TUI redraws entire screen on each interaction — inherent to React/Ink, no config fix
- NODE_PATH env var for prettier.el may be stale

---

## 2026-03-23 (session 3)

### Investigated
- Reviewed .emacs configuration for bugs, stale config, and missing packages
- Compared installed elpa packages against what's referenced in config
- Diagnosed startup warnings: dired `--dired` flag (macOS BSD ls), stale projectile .elc, ivy package-not-found on MELPA

### Changed
- Fixed broken company use-package parenthesization (`:config` block was orphaned outside the form)
- Removed deprecated `company-lsp` (lsp-mode has built-in company integration since 2020)
- Removed dead Marmalade package repo, switched GNU and MELPA to HTTPS
- Added `use-package-always-ensure t` globally, removed redundant per-package `:ensure t`
- Replaced `fill-column-indicator` package with built-in `display-fill-column-indicator-mode` (Emacs 27+)
- Replaced deprecated `lsp-prefer-flymake` with `lsp-diagnostics-provider :flycheck`
- Made `tide-format-before-save` buffer-local to typescript modes (was running globally on all saves)
- Fixed `filladapt` hook typo (`outline-mode-hook` → `outline-mode`), fixed `smex` `:after` quoting
- Removed XEmacs compatibility shim and references
- Removed Dropbox/TaskTracking functions (`tasks`, `projects`, `completed`) and `C-c c` keybinding
- Removed all C/C++ support: clang-format, cmake-mode, clangd config, bsoncxx include path, .h/.hh auto-mode
- Removed redundant `(require 'reftex)` before its use-package block
- Cleaned up all commented-out package blocks and stale `package-selected-packages`
- Added `dired-use-ls-dired nil` for macOS
- Simplified `ask-before-closing` (removed Emacs <21 branch)

### Decisions
- Remove C/C++ and clang support entirely — user no longer uses them
- Remove Dropbox/TaskTracking — stale paths with old macOS capitalization, no longer in use
- Keep smex and counsel config even though not currently installed — user may want them

### Open threads
- Many packages in config are not installed in elpa (forge, magit, counsel, smex, lsp-mode, etc.) — need `M-x package-install-selected-packages` after `M-x package-refresh-contents`
- Stale `.elc` files in elpa — run `M-x byte-recompile-directory` on `~/.emacs.d/elpa/`
- NODE_PATH env var for prettier.el workaround may be stale — worth checking if still needed

---

## 2026-03-23 (session 2)

### Investigated
- Tested dotfiles setup: verified shell, symlinks, plugins, git identity, completions
- Diagnosed `compinit: insecure directories` warning — caused by `/opt/homebrew/share` group-write permissions
- Diagnosed starship prompt overwriting output inside Claude Code's terminal
- Researched Claude Code status line, plugins, hooks, skills, and MCP extensibility

### Changed
- Fixed compinit warning: added `-u` flag to skip insecure directory check (standard Homebrew fix)
- Disabled starship inside Claude Code by gating on `$CLAUDECODE` env var
- Added Claude Code status line showing model, context %, git branch/status, and rate limits
- Added `.claude/statusline.sh` to repo and `install.sh`

### Decisions
- Use `compinit -u` rather than fixing `/opt/homebrew/share` permissions — permissions are set by Homebrew and would revert on updates
- Disable starship entirely in Claude Code rather than tweaking starship config — simpler and avoids all rendering issues
- Rate limit data comes from Claude Code's built-in session JSON, no need for external API calls

### Open threads
- Consider adding notification hook (desktop alert when Claude needs input)
- Consider adding custom skills (e.g., `/test-install` to validate dotfiles setup)
- Status line rate limit fields only appear for Claude.ai subscribers — will be empty for API users

---

## 2026-03-23

### Investigated
- Compared repo state (`~/src/config` and `~/config`) against actual home directory dotfiles
- Identified which tools are still installed (starship, pyenv, emacs) vs removed (gdb, tmate, thefuck, tmux)
- Found symlinks pointing to `~/config/` with uncommitted local changes in .emacs, .bashrc, .bash_profile
- Found `~/.git` symlink making home directory appear to be a git repo
- Current `~/.zshrc` was nearly empty (just LM Studio PATH) — repo version had good structure but many dead references

### Changed
- Committed uncommitted changes in `~/config/` (emacs: disable yasnippet/ivy/golint, add TIDE; bash: add brew/LM Studio PATH)
- Consolidated two repo clones: repointed all symlinks from `~/config/` to `~/src/config/`, backed up `~/config/` to `~/backups/dotfiles-2026-03-23/`
- Removed `~/.git` symlink
- Pruned 9 obsolete files: .bashrc, .bash_profile, .bash_local_osx, .bash_logout, .gdbinit, .tmate.conf, .config/thefuck/settings.py, install_config.sh, install_config_mac.sh
- Rewrote .zshrc: removed dead references (bash-isms, EC2, thefuck, old paths), added homebrew, proper zsh history options, .zshrc.local sourcing
- Updated .gitconfig: removed MongoDB email, added `[include]` for profile-specific identity, fixed `confictStyle` typo
- Added profiles/ directory with personal and work configurations
- Added .claude/CLAUDE.md and settings.json to version control
- Wrote new install.sh that takes a profile name argument
- Updated README.md
- Installed zsh-autosuggestions, zsh-syntax-highlighting, zsh-completions via homebrew and added to .zshrc
- Changed default shell from bash to zsh (`chsh -s /bin/zsh`)

### Decisions
- Use `~/src/config` as the single repo location (user wants all repos in ~/src)
- Delete bash files entirely rather than keeping as reference — all useful content captured in .zshrc
- Keep .tmux.conf symlinked even though tmux isn't installed (harmless, ready for future use)
- Use versioned profiles/ directory for machine-specific config rather than untracked local files — user wants everything in version control
- Git identity (email) comes from profile-specific .gitconfig.local via `[include]`, not hardcoded in shared .gitconfig

### Open threads
- Work profile has placeholder files — fill in when starting new job
- `~/backups/dotfiles-2026-03-23/` contains old `~/config/` backup — can be removed once satisfied
- Could add more files to profiles/ as needs arise (e.g., .claude/ overrides per machine)
