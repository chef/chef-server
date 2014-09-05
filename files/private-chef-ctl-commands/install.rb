# Copyright (c) 2014 Opscode, Inc.
# All Rights Reserved

add_command "install", "Install addon package by name, with optional --path supplied", 2 do
  package = ARGV[3]
  path_arg = "--path"
  if (ARGV.include?(path_arg))
    install_path = ARGV[ARGV.index(path_arg) + 1]
  end

  attributes_path = "#{base_path}/embedded/cookbooks/install_params.json"
  command = ["chef-solo",
    "--config #{base_path}/embedded/cookbooks/solo.rb",
    "--json-attributes #{attributes_path}",
    "--log_level fatal"]

  raise "must supply package name" if (package.nil?)
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
