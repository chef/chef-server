# Copyright (c) 2014 Chef Software, Inc.
# All Rights Reserved

KNOWN_ADDONS = %w{
  chef-manage
  opscode-push-jobs-server
  opscode-reporting
}

add_command_under_category "install", "general", "Install addon package by name, with optional --path parameter indicating directory containing packages", 2 do
  package = ARGV[1]

  # Rewrite deprecated package name to current package name if it
  # happens to appear.
  if package == "opscode-manage"
    STDERR.puts "opscode-manage has been renamed to chef-manage, installing chef-manage instead."
    package = "chef-manage"
  end

  if package == "opscode-reporting"
    STDERR.puts "
     Chef Reporting is a legacy product, and is no longer receiving new feature updates.

     Are you a new Chef customer, or looking to gain better insight into your fleet? Take advantage of Chef Automate.
     You'll get a graphical interface and query language that gives you insight into operational, compliance, and workflow events.
     You can download Chef Automate here: https://downloads.chef.io/automate/

     Your install of Chef Reporting will begin in 5 seconds.
     "
    sleep(5)
  end

  path_arg = "--path"
  if ARGV.include?(path_arg)
    install_path = ARGV[ARGV.index(path_arg) + 1]
  end

  if package.nil?
    STDERR.puts "You must supply an addon name. Valid names include: #{KNOWN_ADDONS.join(", ")}."
    exit 1
  elsif !KNOWN_ADDONS.include?(package)
    STDERR.puts "#{package} does not appear to be a valid addon name. Valid names include: #{KNOWN_ADDONS.join(", ")}."
    exit 1
  end

  attributes_path = "#{base_path}/embedded/cookbooks/install_params.json"
  json_src = { "run_list" => ["recipe[private-chef::add_ons_wrapper]"],
    "private_chef" => { "addons" => {
        "install" => true,
        "packages" => [package],
        "path" => install_path,
      } } }
  File.open(attributes_path, "w") do |file|
    file.write json_src.to_json
  end

  chef_args = "-l fatal"

  status = run_chef(attributes_path, chef_args)
  exit!(status.success? ? 0 : 1)
end
