#
# Copyright:: Copyright (c) 2012 Opscode, Inc.
#
# All Rights Reserved
#

add_command_under_category "test", "general", "Run the API test suite against localhost.", 2 do
  ENV["SUPERUSER_KEY"] = credentials.get("chef-server", "superuser_key")
  ENV["WEBUI_KEY"] = credentials.get("chef-server", "webui_key")
  ENV["STATS_PASSWORD"] = credentials.get("opscode_erchef", "stats_password")

  pedant_args = ARGV[1..-1]
  pedant_args = ["--smoke"] unless pedant_args.any?
  Dir.chdir(File.join(base_path, "embedded", "service", "oc-chef-pedant"))
  pedant_config = File.join(data_path, "oc-chef-pedant", "etc", "pedant_config.rb")
  bundle = File.join(base_path, "embedded", "bin", "bundle")
  # lock down path to exclude other installations of chef and related software.
  ENV["PATH"] = ([File.join(base_path, "embedded", "bin"), File.join(base_path, "bin") ]).join(":")
  status = run_command("#{bundle} exec ./bin/oc-chef-pedant -c #{pedant_config} #{pedant_args.join(" ")}")
  exit status.exitstatus
end
