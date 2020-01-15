# Previously all svlogd processes (which capture STDOUT for all
# private-chef component services) were running as `root` and thus all
# log files were owned by the `root` user. Since the actual processes
# run as the `opscode` user we should ensure the log files match.
#
# Although we updated the `/opt/opscode/sv/*/log/run` scripts that start
# the svlogd processes this doesn't restart/reload the actual logging
# processes. This upgrade script ensures all existing log files are
# owned by the `opscode` user AND restarts all the svlogd processes so
# they are running as the `opscode` user.
define_upgrade do
  %w{
    couchdb
    rabbitmq
    opscode-solr4
    opscode-expander
    opscode-expander-reindexer
    opscode-erchef
    nginx
    postgresql
    redis
    opscode-authz
    bookshelf
    opscode-certificate
    opscode-org-creator
    opscode-account
    nagios
    nrpe
    fcgiwrap
    php-fpm
  }.each do |service|
    if File.exist?("/var/log/opscode/#{service}/")
      # ensure logs are all owned by the opscode user
      run_command("chown opscode:opscode /var/log/opscode/#{service}/*")

      # There could be leftover logs for a service we don't use anymore...
      # (e.g. authz). So check the service is defined before restarting it.
      if File.exist?("/opt/opscode/service/#{service}")
        # force svlogd process to reload
        run_command("/opt/opscode/embedded/bin/sv force-restart /opt/opscode/sv/#{service}/log")
      end
    end
  end

  # Clean up legacy log retention cron job
  run_command("rm -f /etc/cron.d/opc_log_cleanup")
  # Clean up legacy log retention script
  run_command("rm -f /opt/opscode/bin/opc_log_cleanup")
end
