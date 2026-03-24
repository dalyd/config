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
