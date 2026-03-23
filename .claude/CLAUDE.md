# User-level Claude Code Instructions

## WORKLOG.md

Maintain a `WORKLOG.md` file in every project root. This is a chronological session diary that captures the project's evolution so future sessions can pick up where the last one left off.

**When to write:** Update WORKLOG.md at the end of each working session, or after completing a significant piece of work. Create the file if it doesn't exist.

**Each entry should include:**
- **Date** (YYYY-MM-DD)
- **What was investigated** — questions explored, research done, things tried
- **What changed** — files modified, features added, bugs fixed
- **Decisions made and why** — capture the reasoning, not just the outcome
- **Open threads** — what's unfinished, blocked, or should be picked up next

**Format:** Reverse chronological (newest first). Keep entries concise — a few bullet points per section, not paragraphs. Skip sections that don't apply to a given session.

**Example:**
```markdown
## 2026-03-23

### Investigated
- Whether Obsidian CLI `daily` command works with Periodic Notes plugin (it doesn't — requires core Daily Notes plugin configured separately)

### Changed
- Installed obsidian-skills and obsidian-cli-skill plugins for Claude Code
- Enabled daily-notes core plugin

### Decisions
- Use path-based CLI commands for daily notes instead of `daily` subcommand, since vault uses Periodic Notes with custom folder structure
- Use upstream skill plugins rather than copying CLI docs into CLAUDE.md — stays current with updates

### Open threads
- Daily Notes core plugin folder config doesn't match Periodic Notes setup — left unconfigured for now
```
