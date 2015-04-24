define_upgrade do
  log "Removing /opt/opscode/embedded/cookbooks/local-mode-cache"
  run_command("rm -rf /opt/opscode/embedded/cookbooks/local-mode-cache")
end
