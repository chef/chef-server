# Copyright (c) 2014 Chef Software, Inc.
# All Rights Reserved

KNOWN_ADDONS = %w(
  chef-ha
  chef-sync
  opscode-analytics
  opscode-manage
  opscode-push-jobs-server
  opscode-reporting
)

add_command_under_category "install", "general", "Install addon package by name, with optional --path parameter indicating directory containing packages", 2 do
  package = ARGV[3]
  path_arg = "--path"
  if (ARGV.include?(path_arg))
    install_path = ARGV[ARGV.index(path_arg) + 1]
  end

  attributes_path = "#{base_path}/embedded/cookbooks/install_params.json"
  command = ["chef-client -z",
    "--config #{base_path}/embedded/cookbooks/solo.rb",
    "--json-attributes #{attributes_path}",
    "--log_level fatal"]

  if package.nil?
    STDERR.puts "You must supply an addon name. Valid names include: #{KNOWN_ADDONS.join(', ')}."
    exit 1
  elsif !KNOWN_ADDONS.include?(package)
    STDERR.puts  "#{package} does not appear to be a valid addon name. Valid names include: #{KNOWN_ADDONS.join(', ')}."
    exit 1
  end

  json_src = { "run_list" => ["recipe[private-chef::add_ons_wrapper]"],
    "private_chef" => { "addons"=> {
        "install" => true,
        "packages" => [package],
        "path" => install_path
      }}}
  File.open(attributes_path, "w") do |file|
    file.write json_src.to_json
  end

  status = run_command(command.join(" "))
  exit!(status.success? ? 0 : 1)
end
