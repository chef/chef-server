---
name: dependency-inventory
description: Analyze a repository's dependency sources and self-containment, then produce a categorized inventory with precise source references. Use when asked where dependencies come from, whether a project is self-contained, or to document dependencies.
---

# Repository dependency inventory

Analyze the current repository and report whether it is self-contained or
depends on external sources. Do not assume a language or build system: inspect
the repository first, follow its actual components and build paths, and report
only findings supported by repository evidence.

## Discovery

1. Identify the repository root and component/service layout. Note whether
   source code is vendored, included as a submodule, or downloaded during
   build, test, packaging, deployment, or runtime.
2. Find dependency declarations, resolution files, and build/package manifests.
   Check language-specific manifests and locks, workspace or module files,
   imports/includes, package recipes, container definitions, CI/build scripts,
   submodule declarations, and URLs used to fetch source or artifacts. Follow
   relevant references across the repository rather than stopping at the first
   manifest.
3. Distinguish dependency categories:
   - direct declarations versus transitive or resolved entries;
   - development/test dependencies versus production/runtime dependencies;
   - source libraries versus toolchains, system packages, external services,
     prebuilt images, and other build inputs;
   - dependencies checked into the repository versus those fetched externally.
4. For each relevant component, use its lockfile to confirm resolved versions
   or revisions where available. Do not infer a version from a manifest
   constraint if the resolved version is unknown. Say when a lockfile is
   missing, incomplete, or does not pin an input.
5. Inspect whether build definitions vendor, fetch, or package dependencies.
   A lockfile records resolution; it does not by itself mean the dependency
   source is present locally. A monorepo may contain its own components and
   still need external dependencies.
6. If a repository-specific companion file is provided with this skill, read
   it when the target repository matches its scope. Treat it as a discovery
   checklist, not as proof: verify each item in the checked-out repository.
   Ignore it for unrelated repositories.

## Evidence and references

Every dependency claim in the results must have a source reference. Prefer
links to files in the repository, using the repository's web URL and current
revision when available. Otherwise use a repository-relative path. Use exact
line anchors for focused declarations, for example `path/to/file#L12-L15`.
For a lockfile or other file whose relevant content is the whole inventory,
link to the complete file without a line range. Do not cite a whole large file
when only one declaration is relevant.

When a dependency is identified through a source import or include, cite the
specific import/include line. When the source line alone does not reveal the
version or source, cite both it and the manifest/lockfile evidence that does.
Mark unavailable information as unknown rather than guessing. Do not expose
credentials or secret values if found in configuration.

## Output

Write a concise, standalone Markdown report. State the self-containment verdict
and explain what is present in the repository versus what must be fetched or
provided externally. Summarize how dependencies are obtained and point readers
to the authoritative manifests and lockfiles.

Include a dependency reference table grouped by dependency type (for example,
RubyGems, Erlang packages, Habitat packages, system packages, source archives,
container images, or submodules). Use one row per meaningful dependency or
dependency group, with these columns:

| Type | Repository area | Dependency and version/revision | Source/reference |
| --- | --- | --- | --- |
| Example type | Example service or UI | Package name and resolved version, or `unknown` | `[manifest lines](path#L12-L15)`; `[lockfile](path/to/lockfile)` |

Use the actual repository's areas (such as a service, UI, test suite, build,
or runtime). Keep dependencies grouped under clear type headings or group the
table by type. Include direct dependencies and important external inputs; avoid
dumping every transitive lock entry when a whole lockfile reference is clearer.
The table should still make it possible to trace each claim to its declaration
and, where applicable, its resolved version/source. A broad inventory in a
`.lock` file can be linked as a whole; an isolated include or dependency
declaration should link to its exact lines.

Conclude with the best locations to consult for a complete per-component
inventory and note material limitations, such as generated manifests,
uninitialized submodules, private registries, platform-specific dependency
sets, or runtime services not described by build manifests.
