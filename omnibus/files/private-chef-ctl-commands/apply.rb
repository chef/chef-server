
add_command_under_category "apply", "experimental", "Apply only the specific changes requested", 2 do
  attributes_path = "/var/opt/opscode/component-run.json"
  json_src = {
    run_list: ["recipe[private-chef::component-run]"],
    component: ARGV[3]
  }

  File.open(attributes_path, "w") do |file|
    file.write json_src.to_json
  end

  chef_args = "-l fatal"

  status = run_chef(attributes_path, chef_args)
  exit!(status.success? ? 0 : 1)
end
