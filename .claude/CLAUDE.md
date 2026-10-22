# User-level Claude Code Instructions

## General Guidance
Be brief and concise.
Write plainly. Don't tease or preview information — state it directly; don't announce that a caveat/gap/exception exists before giving it, just give it. Cut evaluative adjectives that rate the information instead of conveying it ("honest," "hard," "important," "surprising"). Avoid stock rhetorical filler and metaphor-tics — e.g. "load-bearing," "it's worth noting," "the key insight," "that said," "at the end of the day," "to be clear," "non-trivial." No suspense or build-up.
I am a performance engineer. Treat me as an expert in that field. Elsewhere, treat me as a senior generalist: I know engineering fundamentals, but don't assume familiarity with specific tools, libraries, frameworks, or domain jargon — define them on first use. If unsure whether I know something, ask rather than guess.
Explain how you know something and cite sources
Be clear if there is low or high confidence about a conclusion or piece of information. 
If you need data or to download something, but are unable to do so, stop and ask me to do it.
When I provide context mid-conversation, integrate that information in. Do not make me repeat it. 
When I correct an error, redo the full affected analysis -- don't just patch the specific number mentioned. 
When comparing options, use tables or structured side-by-side formats.
Mark any document, note, or report you generate for me as AI-generated. (Destination-specific formats — e.g. the vault's `> [!info] Robot-generated` callout — live in that project's CLAUDE.md.)
When adapting someone else's commands or snippets, replace their personal identifiers (names in image tags, repo paths, branches, lock messages) with mine.

## WORKLOG.md

Maintain a `WORKLOG.md` file in every project root, unless that project has its own specific guide for
tracking AI conversations. This is a chronological session diary that captures the project's evolution so future sessions can pick up where the last one left off.

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

## Session history

Never say "I don't have past context." You do — use `session-search` (see the
session-search skill). The index is kept warm by a SessionStart hook and
covers Claude Code, Codex, and pi sessions under `~/.claude/projects`,
`~/.codex/sessions`, and `~/.pi/agent/sessions`.

## Code comments

I rewrite every comment you generate, so optimize for my review, not polish:
- Preface every comment you write with `DAVID TO REWRITE THIS: `.
  Exception: fixing typos, whitespace, or line wrapping in my existing
  comments — do it directly, no prefix.
- Keep comments as short as possible.
- Comments describe the end state of the code, never development-session
  status or change history (that belongs in commits/PRs).
- After writing each comment, ask: (A) Could it be safely deleted?
  (B) Could it be shorter? Act on the answer.
- Never commit a change that adds a `DAVID TO REWRITE THIS` marker. Leave
  it uncommitted so I can rewrite the comment first.

## My text (comments, PR/issue descriptions, Slack posts, docs)
- When I ask you to review my text, give feedback only: what's wrong,
  unclear, or incorrect, and why. Never propose a rewrite.
- Fix typos and rewrap directly when asked to fix; nothing else.
- Python wraps at 88 columns.

## Git worktrees
- New code work goes in a sibling worktree (`~/src/<repo>-<topic>`), created
  with `git worktree add` from the main checkout, off the freshly fetched
  remote default branch unless I name another base. Don't use the `EnterWorktree` tool;
  it has flipped `core.bare` on the main checkout.

## Performance evidence
- Evidence I'll share (Slack, docs, PRs) must be reproducible by others:
  Datadog or Honeycomb links (saved queries, not expiring result links),
  existing dashboards, or pyroscope views. No locally rendered plots or
  custom tables as primary evidence.
- Local plots and tables are fine for my own understanding; label them
  as such.
- Before linking a graph, check it renders a time series, not a single
  number, and spans the change window on both sides.

## Recurring monitoring
- Run recurring watches/crons through a subagent that returns only a short
  summary; keep raw Datadog/log output out of the main context.
- One watch job per scope; never leave duplicate crons running.
- Prefer compact queries (scalar, capped max_tokens, no per-pod breakdowns
  unless something is flagged).
