# Chef Infra Server Build Failure Debugging

This document is a reference of build failure patterns for the Chef Infra Server pipeline and suggestions regarding how to proceed to resolve them.

We don't expect this to be a comprehensive list of every possible build failure. This document is organized by the type of failure to group error messages that have common areas of further investigation.


## License Scout

The [license\_scout](https://github.com/chef/license_scout) project is used to ensure only approved software licenses are used in the software that we distribute.

Chef Infra Server still uses the v1 release of license\_scout, from the [1-stable branch](https://github.com/chef/license_scout/tree/1-stable).

#### Error Message
```
    Dependency 'coolline' version '0.5.0' under 'ruby_bundler' is missing license information.
```

#### Next Steps

These errors often occur when we introduce a new library as a dependency, or an existing dependency introduces another dependency. The 'ruby\_bundler' portion indicates this is a Rubygem dependency. In most cases license\_scout is unable to determine the license of the project and we must provide that for it in `lib/license_scout/overrides.rb` of the license\_scout repository.

First, find the project's official source code repository. With a Rubygem, you can usually go to https://rubygems.org/gems/GEM\_NAME, e.g. https://rubygems.org/gems/coolline and look for a homepage link.

Now we look for license information in the source code. Often there is a LICENSE file or variation, as in this case: https://github.com/Mon-Ouie/coolline/blob/master/LICENSE. Later we specify the name of this file so that all licenses are included with our builds.

Next, we identify the license itself. In this case it doesn't say in the license which license it is. If we search for a sentence from the file, we typically find the license text on opensource.org.

For this project, this leads us to the [Zlib license](https://opensource.org/licenses/Zlib). We *must* ensure the language matches exactly. If any word is changed or left out then the license is modified and has to be considered individually. Some licenses have places for the copyright holders to put their names in the license text. This is allowed. In this case, the coolline license matches the Zlib license.

Finally the `lib/license_scout/overrides.rb` file needs to be updated with this information. Look for the section that has the array of gems and add a new line in alphabetic order.

```
["gem name", "license name", [["file names that contain the license(s)"]],
["coolline", "Zlib", [["LICENSE"]],
```

The first field must match the name of the gem, the second should be the [SPDX Identifier for the license](https://spdx.org/licenses/) if one exists.

If the license file exists in the online repository but not in the Rubygem, it can be listed with the URL to the file in the second half of the list in `lib/license_scout/overrides.rb`.

If the license is one of these, it is pre-approved. If it is not in this list or modified at all, it must be escalated to a manager for followup.

Preapproved: Apache-2.0, BSD-2-Clause, BSD-3-Clause, Chef-MLSA, CPL-1.0, ISC, MIT, Public-Domain, PostgreSQL, Ruby, X11
Allowed for distribution, but not for use in our products: Artistic-1.0, Artistic-2.0, EPL-\*, GPL-1.0-\*, GPL-2.0-\*, LGPL-\*, MPL-\*, OpenSSL, Perl-5
Not allowed: AGPL-\*, GPL-3.0-\*.

Ask a manager if anything around licensing isn't explicitly clear

#### Error Message

```
[Licensing] W | 2022-01-10T07:38:46+00:00 | Can not automatically detect licensing information for 'sqitch' using license_scout. Error is: 'Provided license file path 'README' can not be found under detected dependency path '/var/lib/buildkite-agent/.cpanm/latest-build/ExtUtils-ParseXS-3.44'.'

Encountered error(s) with project's licensing information.
Failing the build because :fatal_licensing_warnings is set in the configuration.
Error(s):

    Can not automatically detect licensing information for 'sqitch' using license_scout. Error is: 'Provided license file path 'README' can not be found under detected dependency path '/var/lib/buildkite-agent/.cpanm/latest-build/ExtUtils-ParseXS-3.44'.'

    If you are encountering missing license or missing license file errors for **transitive** dependencies, you can provide overrides for the missing information at https://github.com/chef/license_scout/blob/1-stable/lib/license_scout/overrides.rb#L93.
 Promote license_scout to Rubygems with `/expeditor promote chef/license_scout:1-stable X.Y.Z` in slack.

/var/lib/buildkite-agent/builds/omnibus-ubuntu-1604-x86-64-prod-i-0a3a3e06444edd56e-1/chef/chef-chef-server-main-omnibus-adhoc/omnibus/vendor/bundle/ruby/2.7.0/bundler/gems/omnibus-2bf77bb5515a/lib/omnibus/licensing.rb:446:in `raise_if_warnings_fatal!'
  /var/lib/buildkite-agent/builds/omnibus-ubuntu-1604-x86-64-prod-i-0a3a3e06444edd56e-1/chef/chef-chef-server-main-omnibus-adhoc/omnibus/vendor/bundle/ruby/2.7.0/bundler/gems/omnibus-2bf77bb5515a/lib/omnibus/licensing.rb:545:in `rescue in collect_transitive_dependency_licenses_for'
  /var/lib/buildkite-agent/builds/omnibus-ubuntu-1604-x86-64-prod-i-0a3a3e06444edd56e-1/chef/chef-chef-server-main-omnibus-adhoc/omnibus/vendor/bundle/ruby/2.7.0/bundler/gems/omnibus-2bf77bb5515a/lib/omnibus/licensing.rb:515:in `collect_transitive_dependency_licenses_for'
  /var/lib/buildkite-agent/builds/omnibus-ubuntu-1604-x86-64-prod-i-0a3a3e06444edd56e-1/chef/chef-chef-server-main-omnibus-adhoc/omnibus/vendor/bundle/ruby/2.7.0/bundler/gems/omnibus-2bf77bb5515a/lib/omnibus/licensing.rb:148:in `execute_post_build'
```
#### Next Steps

Edit `lib/license_scout/overrides.rb` and change `["ExtUtils-ParseXS", "Perl-5", ["README"]]` to `["ExtUtils-ParseXS", "Perl-5", ["META.json"]]`.

### Other Places to Look

Some license scout errors may require edits to the following files in order to repair:

Repo  - omnibus-software  
Files - config/software/sqitch.rb

Repo  - chef-server  
Files - omnibus/Gemfile  
        omnibus/Gemfile.lock

A trick to avoid having to make changes to the chef-server repo to point to your omnibus-software changes while testing the fix is to set OMNIBUS_SOFTWARE_GITHUB_BRANCH=YOUR-OMNIBUS-SOFTWARE-BRANCH-NAME under the Options - Environment Variables section of the buildkite omnibus/adhoc GUI.  As you are avoiding having to create a custom branch to point to your omnibus-software changes, leave the branch set to main.
