# Contributing to Chef Server

In order to facilitate the tedious communication tasks involved in releasing Chef Server, we ask that all contributors adhere to a **"publicize as you go"** policy. Each pull request to `opscode-omnibus` should be accompanied by an update to [CHANGELOG.md](CHANGELOG.md) and, if necessary, [RELEASE_NOTES.md](RELEASE_NOTES.md), or a note in the PR explaining that such an update is unnecessary. Changes to these files should occur **above** the most recent release and will be rolled up into the communication for the next release.

## Changelog

The CHANGELOG.md file is intended to provide a concise list of downstream project changes for development and support purposes only. All relevant changes to `opscode-omnibus` should be represented in the CHANGELOG.md. 

## Release Notes

The RELEASE_NOTES.md file can be viewed as a higher-level, customer-friendly version of CHANGELOG.md. Significant changes to `opscode-omnibus` and its dependencies should be represented in one of the following three categories:
* What's New
* Bug Fixes
* Security Fixes

These notes will be included as part the blog post for each release.