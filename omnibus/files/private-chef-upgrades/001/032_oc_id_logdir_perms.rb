# oc_id's svlogd process running as root looks like a leftover of migration
# 008_fix_logging.rb -- so we do the same thing for oc_id's log dir
define_upgrade do
  service = "oc_id"
  if File.exist?("/var/log/opscode/#{service}/")
    # restart log service
    run_command("/opt/opscode/embedded/bin/sv force-restart /opt/opscode/sv/#{service}/log")
  end
end
