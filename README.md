# config

Personal configuration files (dotfiles), managed with symlinks.

## Setup

Clone the repo and run the install script with a profile name:

```bash
git clone git@github.com:dalyd/config.git ~/src/config
cd ~/src/config
./install.sh personal
```

## Structure

- **Shared core** (top-level dotfiles): `.zshrc`, `.emacs`, `.gitconfig`, `.tmux.conf`, etc.
- **`profiles/`**: Machine/context-specific overrides. Each profile directory contains `.local` files that are sourced or included by the shared core.
  - `profiles/personal/` — personal machine settings
  - `profiles/work/` — work machine settings
- **`install.sh <profile>`**: Symlinks shared core + profile overrides into `~/`

## Adding a new profile

1. Create a directory under `profiles/` (e.g., `profiles/newjob/`)
2. Add `.gitconfig.local` and `.zshrc.local` with context-specific settings
3. Run `./install.sh newjob`
