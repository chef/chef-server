# Code Review Check List

The goal of the code review checklist is to start productive questions
during code review to help ensure that we are constantly improving the
quality of the Chef Server.

Change authors should review the checklist before submitting their
pull request. Reviewers should consult the checklist during review in
addition to reviewing the code for any language-specific standards we
may have.

- [ ] Do the commit messages and PR description give enough context on why
      this change is being made? Would someone reading the commit message
      6 months from now find it useful?

- [ ] Does the code work? Does it compile without warnings?

- [ ] Are the travis-based unit tests and the pedant test passing?

- [ ] Are there integration tests for new user-facing features?

- [ ] If this is a bug fix, is there a regression test for the bug?

- [ ] Are the included tests likely to prevent future breakage of this
      feature?

- [ ] Has this PR changed the format of any on disk files that may be
      used by add-ons? If so, has add-on compatibility been
      specifically tested?

- [ ] Has this PR changed an API response that may break API clients?
      If so, does this change require an API version bump and has time
      been set aside to update chef-client?

- [ ] Are there constants that should be configurable?

- [ ] Do any new configurables have a sensible default value?

- [ ] Are any new user-visible settings/features documented? (via a
      linked PR to [chef-web-docs](https://github.com/chef/chef-web-docs/))

- [ ] Is there enough logging or other diagnostic information in the
      code to aid future debugging?

- [ ] Can any of the logging be removed or moved to a more detailed
      log level?

- [ ] Does this change introduce new dependencies? If so, do we
      believe those dependencies are necessary and stable? Are all new
      dependencies included in the omnibus package OR documented as
      things we expect on the hosts (e.g. OpenSSH)?

- [ ] Does this change break the dev environment? If so, can we fix it
      now or have we prioitized time to fix it in the near future?

- [ ] Are failures from external systems or libraries handled? If not,
      do we know what the behavior will be in the case of failure? Is
      that the behavior we want?

- [ ] Are variable and function names clear? Do functions/methods have
      a clear purpose?

- [ ] Is it clear how the code in the change addresses the stated goal
      of the pull request?

- [ ] Are there compromises this PR is making because of time or other
      constraints? If so, are those compromises documented in `TODO`
      or `NOTE` comments?

- [ ] Do any Preflight checks account for existing installations?

- [ ] Will this change work on all of our supported platforms and
      browsers?

- [ ] Do we expect significant changes in performance due to the
      changed code? Do we have any indication of the impact related to
      those performance changes?

- [ ] Should this change be reflected in the release notes? If so, has
      RELEASE_NOTES.md been updated?
