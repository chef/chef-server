# Copyright (c) 2013 Opscode, Inc.
# All Rights Reserved

add_command_under_category "cleanup", "general", "Perform post-upgrade removal of now-obsolete data, configuration files, logs, etc.  Add the '--no-op' flag to see what *would* be removed.", 2 do
  use_why_run_mode = ARGV.include?("--no-op")

  # Our cleanup process is really just a special chef run
  command = ["chef-client -z",
             "--config #{base_path}/embedded/cookbooks/solo.rb",
             "--json-attributes #{base_path}/embedded/cookbooks/post_upgrade_cleanup.json",
             "--log_level fatal"] # yes, that's an underscore in log_level
  command << "--why-run" if use_why_run_mode

  status = run_command(command.join(" "))
  exit!(status.success? ? 0 : 1)
end
