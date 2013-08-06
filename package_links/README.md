Automatically Generate Tech Preview Product Links
=================================================

# For the Impatient...

``` sh
export AWS_ACCESS_KEY_ID='...'
export AWS_SECRET_ACCESS_KEY='...'
bundle install
bundle exec ./generate_wiki_page.rb | pbcopy
# paste into wiki
```

# Background

We store "tech preview" builds in a private S3 bucket for our main
products (Private Chef, Reporting, Pushy, Web UI, and associated
`knife` plugins).  These links are used by the sales department and
others in the course of talking with and presenting demos to
prospective clients.  These links need to expire, though, and
maintaining the
[wiki page](http://wiki.corp.opscode.com/display/CORP/Private+Chef+11+Preview+Builds)
manually is a special kind of hell.

This script automates the process: supply it with our current AWS
"preprod" credentials (see [Teampass](http://teampass.opscode.com))
via environment variables and it will generate the full Confluence
wiki markup for the above-linked page, complete with freshly-minted
URLs that expire (by default) in 7 days.  When it's time to refresh
the URLs, just run the script and paste the entire output into the
wiki and hit "Save".

Apart from AWS credentials, the other main inputs to the script are a
list of packages to create links for, as well as an S3 bucket name
("opc11-tech-preview") and a default "time to live" value for the
links (currently 7 days).  As these are relatively stable, they're
built into the script.  As new preview builds are generated, the
package list should be updated as needed.  You only need a list of
package names (no deeply-nested hashes in JSON, as with Omnitruck);
the product and platform information are extracted from the name.
