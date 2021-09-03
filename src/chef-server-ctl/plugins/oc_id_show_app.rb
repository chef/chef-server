add_command_under_category "oc-id-show-app", "Secrets Management", "Show configuration for oc-id applications", 2 do
  require "mixlib/shellout"
  require "json"

  app_name = ARGV[3]
  unless app_name
    STDERR.puts "No app name provided"
    exit(1)
  end

  rails_script = <<EOF
app = Doorkeeper::Application.find_by(:name => "#{app_name}");
puts app.to_json
EOF

  env_helper = "veil-env-helper --use-file -s chef-server.webui_key -s oc_id.sql_password -s oc_id.secret_key_base"
  cmd = Mixlib::ShellOut.new("#{env_helper} -- bin/rails runner -e production '#{rails_script}'",
                          cwd: "/opt/opscode/embedded/service/oc_id")
  cmd.run_command
  json = cmd.stdout.lines.last.chomp
  if json == "null"
    STDERR.puts "Could not find app #{app_name}"
    exit(1)
  end

  hash = JSON.parse(json).delete_if { |key| %w{ id created_at updated_at}.include? key }
  puts JSON.pretty_generate(hash)
end
