# Contributing to Chef Server

Thank you for your interest in Chef Server!  We use **GitHub Issues**
for issue tracking and contributions:

1. Reporting an issue or making a feature request
2. Submitting patches for new features or bug fixes

## Submitting Patches

All patches should be submitted as GitHub pull requests:

1. Sign or be added to an existing
[Contributor License Agreement (CLA)](https://supermarket.getchef.com/become-a-contributor).

2. Create a GitHub Pull Request.

3. If you are a Chef Software employee, submit an ad-hoc
chef-server-12 build for your branch and post a link to in the pull
request. For non Chef Software employees, your job will be submitted
for you by one of the reviewers.

4. Respond [Code Review](#cr) to code review comments to improve the
code until it is ready to merge.

5. Merge it! (If you don't have commit access, a maintainer will merge
   it for you after code review is complete).

## Reporting an Issue

When reporting an issue, please try to include as much detail as
possible, including:

- The version of the Chef Server you are running,
- The operating system you are running it on,
- What you are trying to achieve but can't,
- The steps to reproduce the issue you are seeing,
- What you expect to see given the steps you've described, and
- What you actually see given the steps you've described.

## <a name="cr"></a> Code Review Process

The Chef Code Review process happens on Github pull requests. See
[this article](https://help.github.com/articles/using-pull-requests)
if you're not familiar with Github Pull Requests.

Once you a pull request, the **Chef Engineering Team** or **Chef Server
Committers** will review your code and respond to you with any
feedback they might have.

The goal of code review is to work together to improve the quality of
the code in the Chef Server code base. To get the most out of code
review, please try to:

1. When possible, keep pull requests small (200-400) lines of
   changes. If a small change isn't possible, try to keep individual
   commits small.

2. Write informative commit messages during development. The best
   commit messages will describe what changed at a high level and why
   the change was made.

3. Leave an initial pull request comment with any additional
   information that you think might help reviewers but isn't
   appropriate for commit messages.

The process at this point is as follows:

1. 2 thumbs-ups are required from the **Chef Engineering Team** or
**Chef Server Committers** for all merges.

2. When ready, your pull request will be tagged with label `Ready For
   Merge`.

3. Your patch will be merged into `master` including necessary
  documentation updates and you will be included in
  `CHANGELOG.md`. Our goal is to have patches merged in 2 weeks after
  they are marked to be merged.
