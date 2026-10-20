---
name: release-changes
description: Collect the pull requests and Jira issues that went into a release (from the Expeditor CHANGELOG.md or from git history), enrich them with GitHub labels and changed files, and classify each one as feature, improvement, bug fix, security, dependency, or excluded. Use this to gather inputs for release notes, or to answer "is PR/Jira X in release Y".
---

# release-changes

## Steps

1. **Find the range.** `--from` is the previous **published** release (exclusive). `--to` is this release (inclusive) and defaults to the `VERSION` file. For chef-server, find the previous stable release with the `<!-- latest_stable_release -->` marker in `CHANGELOG.md`, or the latest "Chef Infra Server X" heading on the docs site.
2. **Collect.** Pass `--github` whenever `gh` is authenticated. Labels and changed files make classification much better.
   ```bash
   python3 .github/skills/release-changes/scripts/release_changes.py --product chef-server \
     --from 15.10.125 --to 15.10.127 --github -o /tmp/changes.json
   ```
   For products without an Expeditor changelog, use `--source git`. It reads squash-merge subjects like `Title (#123)` between the two tags.
3. **Add Jira data (recommended).** For every key in `prs[].jira`, fetch the issue with the Jira/Atlassian MCP tools. Write a file shaped like this:
   ```json
   {"CHEF-37962": {"type": "Bug", "summary": "...", "status": "Done", "customer_facing": "..."}}
   ```
   Re-run with `--jira /tmp/jira.json`. The Jira issue type takes precedence over labels and title keywords.
4. **Review the classification.** Check every `unclassified` item, and check `excluded` items for false positives. Override decisions in the release-notes `notes.json` file (`prs.<number>.section`) instead of editing the script.

## Classification order

If a PR body has a filled-in `### Release Note` section (from `.github/PULL_REQUEST_TEMPLATE.md`), that text becomes the default customer-facing wording. A release note of "none" excludes the PR.

The first rule that matches wins:

1. **Excluded:** the PR has an excluded label, its title matches an exclusion pattern, or it only touches non-shipping paths.
2. **Jira issue type:** mapped to a section by `jira_type_sections`.
3. **PR labels:** mapped by `label_sections`.
4. **CVE or GHSA ID** in the title or body → **security**.
5. **Dependency files changed** (lockfiles, `omnibus_overrides.rb`, `plan.sh`, the omnibus submodule) → **dependency**.
6. **Title keyword regexes** (`title_keyword_sections`).
7. Otherwise → **unclassified**.

All of these rules live in the product config under `classification`.
