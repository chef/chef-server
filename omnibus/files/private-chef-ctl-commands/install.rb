# Copyright (c) 2014 Chef Software, Inc.
# All Rights Reserved

KNOWN_ADDONS = %w(
  chef-ha
  chef-manage
  opscode-push-jobs-server
  opscode-reporting
)

add_command_under_category "install", "general", "Install addon package by name, with optional --path parameter indicating directory containing packages", 2 do
  package = ARGV[3]

  # Rewrite deprecated package name to current package name if it
  # happens to appear.
  if package == "opscode-manage"
    STDERR.puts "opscode-manage has been renamed to chef-manage, installing chef-manage instead."
    package = "chef-manage"
  end

  path_arg = "--path"
  if (ARGV.include?(path_arg))
    install_path = ARGV[ARGV.index(path_arg) + 1]
  end

  if package.nil?
    STDERR.puts "You must supply an addon name. Valid names include: #{KNOWN_ADDONS.join(', ')}."
    exit 1
  elsif !KNOWN_ADDONS.include?(package)
    STDERR.puts  "#{package} does not appear to be a valid addon name. Valid names include: #{KNOWN_ADDONS.join(', ')}."
    exit 1
  end

  attributes_path = "#{base_path}/embedded/cookbooks/install_params.json"
  json_src = { "run_list" => ["recipe[private-chef::add_ons_wrapper]"],
    "private_chef" => { "addons"=> {
        "install" => true,
        "packages" => [package],
        "path" => install_path
      }}}
  File.open(attributes_path, "w") do |file|
    file.write json_src.to_json
  end

  chef_args = "-l fatal"

  status = run_chef(attributes_path, chef_args)
  exit!(status.success? ? 0 : 1)
end
