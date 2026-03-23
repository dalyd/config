#!/bin/bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

usage() {
    echo "Usage: $0 <profile>"
    echo "  profile: name of a directory under profiles/ (e.g., personal, work)"
    echo ""
    echo "Available profiles:"
    for d in "$SCRIPT_DIR"/profiles/*/; do
        echo "  $(basename "$d")"
    done
    exit 1
}

if [ $# -ne 1 ]; then
    usage
fi

PROFILE="$1"
PROFILE_DIR="$SCRIPT_DIR/profiles/$PROFILE"

if [ ! -d "$PROFILE_DIR" ]; then
    echo "Error: profile '$PROFILE' not found at $PROFILE_DIR"
    usage
fi

# Backup a file if it exists and is not already a symlink
backup() {
    local target="$1"
    if [ -e "$target" ] && [ ! -L "$target" ]; then
        local backup="${target}.bak.$(date +%Y%m%d%H%M%S)"
        echo "  Backing up $target -> $backup"
        mv "$target" "$backup"
    fi
}

# Create a symlink, backing up any existing non-symlink file
link() {
    local src="$1"
    local dst="$2"

    # Create parent directory if needed
    mkdir -p "$(dirname "$dst")"

    backup "$dst"

    # Remove existing symlink if present
    [ -L "$dst" ] && rm "$dst"

    ln -s "$src" "$dst"
    echo "  $dst -> $src"
}

echo "Installing config with profile: $PROFILE"
echo ""

# Shared core files
echo "Linking shared config:"
link "$SCRIPT_DIR/.zshrc"              "$HOME/.zshrc"
link "$SCRIPT_DIR/.emacs"              "$HOME/.emacs"
link "$SCRIPT_DIR/.emacs.d"            "$HOME/.emacs.d"
link "$SCRIPT_DIR/.gitconfig"          "$HOME/.gitconfig"
link "$SCRIPT_DIR/.gitignore_global"   "$HOME/.gitignore_global"
link "$SCRIPT_DIR/.tmux.conf"          "$HOME/.tmux.conf"
link "$SCRIPT_DIR/.config/starship.toml" "$HOME/.config/starship.toml"
link "$SCRIPT_DIR/.claude/CLAUDE.md"   "$HOME/.claude/CLAUDE.md"
link "$SCRIPT_DIR/.claude/settings.json" "$HOME/.claude/settings.json"
echo ""

# Profile-specific overrides
echo "Linking profile overrides ($PROFILE):"
for file in "$PROFILE_DIR"/.*; do
    basename="$(basename "$file")"
    # Skip . and ..
    [ "$basename" = "." ] || [ "$basename" = ".." ] && continue
    link "$file" "$HOME/$basename"
done
echo ""

echo "Done. Profile '$PROFILE' installed."
