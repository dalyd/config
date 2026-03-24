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
