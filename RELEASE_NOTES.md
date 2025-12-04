# Chef Infra Server Release Notes

See [Chef Infra Server Release Notes](https://docs.chef.io/release_notes_server) for the complete list of product release notes.

## Unreleased

### Bug Fixes

- **CHEF-28245**: Fixed `chef-server-ctl user-list -a` and `--all-info` flags
  - The `-a` and `--all-info` flags for `chef-server-ctl user-list` now work correctly after the knife-opc plugin removal in Chef Server 15.10.63+
  - These flags are internally transformed to the native knife `--verbose` flag, which retrieves detailed user information (email, first_name, last_name, display_name)
  - Maintains backward compatibility for existing scripts and pipelines using these flags
  - Addresses customer-reported regression where the flags were silently ignored instead of providing detailed user data
  
### Important Notes

- In Chef Server 15.10.63+, the `knife-opc` plugin was removed in favor of native knife commands
  - `chef-server-ctl user-list -a` / `--all-info` now uses native knife's `--verbose` flag internally
  - Existing command-line interfaces remain unchanged for backward compatibility
  - Scripts using `-a` or `--all-info` will continue to work without modification

