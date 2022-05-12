Run `chef-backend-ctl gen-sample-backend-config` to generate the `chef-backend.rb` file.
This will control most of the various feature and configuration flags going into
a Chef HA backend node. A number of these options control the reliability, stability, and
uptime of the backend PostgreSQL databases, the Elasticsearch index,
and the leader election system. Please refrain from changing them unless
you have been advised to do so.

The following settings are the only settings you should modify without guidance:
`fqdn`
: Host name of this node.

`hide_sensitive`
: Set to `false` if you wish to print deltas of
  sensitive files and templates during `chef-backend-ctl reconfigure` runs.
: Default value: `true`.

`ip_version`
: Set to either `'ipv4'` or `'ipv6'`.
: Default value: `'ipv4'`.

`publish_address`
: Externally resolvable IP address of this back-end node.
