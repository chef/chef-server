
ruby_block
execute "setting up host FS syncing" do
  block: begin
           # Or should we just cron
           # it -
           #  - capture run results (NOTE! WE're not constrained to limimted mac osx output anymore!)
           #  -   imclude meaningful exit code parsing
           #  - COMMANDS:
           #    dvm host-sync status
           #    dvm host-sync resume
           #    dvm host-sync pause
           #    dvm host-sync now
           #    dvm host-sync last
           #
           #
   #  exec "/opt/opscode/embedded/bin/ruby /vagrant/sync"
  end

end

