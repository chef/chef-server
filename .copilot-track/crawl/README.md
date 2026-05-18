# Copilot Crawl — README

This folder is the **home base** for AI-assisted incremental development on Chef Server.
It captures the conventions that let Copilot (and human reviewers) follow the chain of
work across multiple PRs, reproduce decisions, and audit AI involvement.

---

## 1. Chain-PRs

Large features are broken into a **sequential chain of small pull requests**, each building
on the previous merge commit.

```
main ──┬── feat/TICKET-step-1  (merged)
       │        │
       └────────┴── feat/TICKET-step-2  (open, base = step-1)
                         │
                         └── feat/TICKET-step-3  (draft, base = step-2)
```

Rules:
- **One logical change per PR** — reviewers should be able to understand the diff in < 15 min.
- **Base branch = previous step**, not `main`, until the chain is ready to land.
- Each PR title begins with the Jira ID and step number:
  `[TICKET-123] Step 2/4 — Add bifrost ACL migration`
- When a parent PR is force-pushed or rebased, **repair the chain** before requesting review
  (see the `sha-cascade-repair` agent in Copilot for automated help).
- Merge order is strictly sequential; never merge step N+1 before step N.

---

## 2. Evidence in PRs

Every AI-assisted PR must include a structured **Evidence block** in its description so that
reviewers and auditors can trace what was generated vs. hand-written.

### Minimum evidence block

```markdown
### AI Assistance Evidence
| Item | Detail |
|---|---|
| **Model** | Claude Sonnet 4.6 (claude-sonnet-4.6) |
| **Prompt summary** | Created bifrost schema migration for new acl_v2 table |
| **Files generated** | `src/oc_bifrost/schema/deploy/acl_v2.sql` |
| **Files hand-edited post-generation** | `src/oc_bifrost/rebar.config` (version pin) |
| **Tests added** | `src/oc_bifrost/test/bifrost_acl_v2_tests.erl` |
| **Coverage delta** | +3 % (87 % → 90 %) |
| **Compliance label** | `ai-assisted` label applied ✅ |
| **Jira field** | `customfield_11170` set to "Yes" ✅ |
```

- Attach the **raw Copilot session plan** (`plan.md`) as a PR comment or gist link when the
  task spanned multiple turns.
- If the AI produced code that was **rejected or significantly rewritten**, note that too —
  it improves future prompts.

---

## 3. Prompt Usage

### Writing effective prompts for this codebase

| Do | Don't |
|---|---|
| Reference specific file paths and service names | Ask for "the server code" without context |
| Specify the target language (Erlang / Ruby) and test framework | Leave the framework ambiguous |
| Ask for one logical unit at a time | Bundle schema + API + tests in one giant prompt |
| Include "do not modify submodules or vendor folders" | Assume the AI knows repo boundaries |
| Mention the relevant Jira ID for traceability | Describe work without a ticket reference |

### Recommended prompt template

```
Context: Chef Server repo, service: <oc_erchef|oc_bifrost|bookshelf|oc-id|chef-server-ctl>
Jira: <TICKET-ID>
Task: <one clear sentence>

Constraints:
- Do not modify files under omnibus/ (submodule) or any vendor/ directory
- Follow existing code patterns in src/<service>/
- Add EUnit / RSpec tests for all new code
- Target ≥ 85 % line coverage

Output: source file(s) + test file(s) + a brief summary of what changed and why
```

### Iterating on generated code

1. Review the diff with `git diff` before accepting.
2. Run `make all` (Erlang) or `bundle exec rspec` (Ruby) locally.
3. If tests fail, paste the failure output back to the AI with: *"Fix the failing tests above
   without changing the public API"*.
4. Commit only after all quality gates pass (see [`build-test.md`](../../ai-track-docs/build-test.md)).

---

## 4. Session Artifacts

Files placed in this folder (`.copilot-track/crawl/`) are **not deployed** and are excluded
from packaging. They exist solely for AI context and audit purposes:

| File | Purpose |
|---|---|
| `README.md` | This file — conventions and workflow |
| `plan-<TICKET>.md` | Per-ticket implementation plan from Copilot |
| `session-<date>.log` | Optional: exported Copilot session transcript |

---

## 5. Quick Reference Links

- System overview → [`ai-track-docs/SYSTEM-OVERVIEW.md`](../../ai-track-docs/SYSTEM-OVERVIEW.md)
- Build & test guide → [`ai-track-docs/build-test.md`](../../ai-track-docs/build-test.md)
- Architecture diagram → [`ai-track-docs/architecture.mmd`](../../ai-track-docs/architecture.mmd)
- Contributing guide → [`CONTRIBUTING.md`](../../CONTRIBUTING.md)
- Code review checklist → [`CODE_REVIEW_CHECKLIST.md`](../../CODE_REVIEW_CHECKLIST.md)
