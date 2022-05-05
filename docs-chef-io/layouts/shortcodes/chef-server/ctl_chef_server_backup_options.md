This subcommand has the following options:

`-y`, `--yes`

:   Use to specify if the Chef Infra Server can go offline during
    tar.gz-based backups.

`--pg-options`

:   Use to specify and pass additional options PostgreSQL during backups. See the [PostgreSQL documentation](https://www.postgresql.org/docs/13/runtime-config.html) for more information.

`-c`, `--config-only`

:   Backup the Chef Infra Server configuration **without** backing up data.

`-t`, `--timeout`

:   Set the maximum amount of time in seconds to wait for shell commands (default 600). This option should be set to greater than 600 for backups taking longer than 10 minutes.

`-h`, `--help`

:   Show help message.
