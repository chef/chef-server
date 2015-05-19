# Copyright (c) 2013 Opscode, Inc.
# All Rights Reserved

add_command_under_category "cleanup", "general", "Perform post-upgrade removal of now-obsolete data, configuration files, logs, etc.  Add the '--no-op' flag to see what *would* be removed.", 2 do
  use_why_run_mode = ARGV.include?("--no-op")

  attributes_path = "#{base_path}/embedded/cookbooks/post_upgrade_cleanup.json"

  chef_args = "-l fatal"
  chef_args << " --why-run" if use_why_run_mode

  status = run_chef(attributes_path, chef_args)
  exit!(status.success? ? 0 : 1)
end
