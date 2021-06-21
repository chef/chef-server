# Chef Infra Server Release Cadence

Chef Infra Server follows [Semantic Versioning](https://semver.org/) for releases. Major versions (eg. 13.x -> 14.x) will include backwards-incompatible changes or changes that require migration events which may include significant downtime. Minor versions (eg 14.1 -> 14.2) will include new features and bug fixes, but will be backwards-compatible to the best of our ability. Patch releases will contain bug and security fixes only.

Chef Infra Server feature releases are promoted to the stable channel once per month. It is expected that this occur during the last week of the month unless circumstances intervene. Additional patch releases for a given feature release may be promoted if critical issues are found.

## Rationale

Monthly releases help ensure we get new features and minor bug fixes out to Chef Infra users in a timely fashion while not overloading the maintainer teams.
