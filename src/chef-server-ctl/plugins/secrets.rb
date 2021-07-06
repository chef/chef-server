require "mixlib/shellout"

add_command_under_category "set-db-superuser-password", "Secrets Management", "Add or change DB superuser password", 2 do
  confirm_continue!("WARN: Manually setting the DB superuser password is only supported for external postgresql instances")
  password = capture_secret_value("DB_PASSWORD", "DB superuser password", ARGV[1])
  set_secret_("postgresql", "db_superuser_password", password)
end

KNOWN_CREDENTIALS = {
  "ldap" => ["bind_password"],
  "data_collector" => ["token"],
  "redis_lb" => ["password"],
  "drbd" => ["shared_secret"],
  "keepalived" => ["vrrp_instance_password"],
  "opscode_erchef" => %w{sql_password sql_ro_password stats_password},
  "oc_bifrost" => %w{superuser_id sql_password sql_ro_password},
  "oc_id" => %w{secret_key_base sql_password sql_ro_password},
  "bookshelf" => %w{access_key_id secret_access_key sql_password sql_ro_password},
  "manage" => %w{secret_key_base secret_token},
  "saml" => %w{client_id client_secret},
  "push-jobs-server" => %w{pushy_priv_key pushy_pub_key sql_password sql_ro_password},
  "opscode-reporting" => %w{sql_password sql_ro_password},
}

SERVICES_REQUIRING_RESTART = {
  "bookshelf.access_key_id" => %w{opscode-erchef bookshelf},
  "bookshelf.secret_access_key" => %w{opscode-erchef bookshelf},
  "bookshelf.sql_password" => ["bookshelf"],
  "chef-server.superuser_key" => ["opscode-reporting"],
  "chef-server.webui_key" => ["oc_id"],
  "chef-server.webui_pub_key" => %w{opscode-erchef opscode-reporting},
  "data_collector.token" => %w{opscode-erchef nginx},
  "ldap.bind_password" => ["opscode-erchef"],
  "manage.secret_key_base" => ["chef-manage"],
  "manage.secret_token" => ["chef-manage"],
  "oc_bifrost.sql_password" => ["oc_bifrost"],
  "oc_bifrost.superuser_id" => %w{opscode-erchef oc_bifrost opscode-chef-mover},
  "oc_id.secret_key_base" => ["oc_id"],
  "oc_id.sql_password" => ["oc_id"],
  "opscode-reporting.sql_password" => ["opscode-reporting"],
  "opscode_erchef.sql_password" => %w{opscode-erchef opscode-chef-mover},
  "push-jobs-server.pushy_priv_key" => ["opscode-push-jobs-server"],
  "push-jobs-server.pushy_pub_key" => ["opscode-push-jobs-server"],
  "push-jobs-server.sql_password" => ["opscode-push-jobs-server"],
  "redis_lb.password" => %w{opscode-chef-mover nginx redis_lb},
  "saml.client_id" => ["chef-manage"],
  "saml.client_secret" => ["chef-manage"],
}

MANAGE_SVDIR = "/opt/chef-manage/sv/"

add_command_under_category "show-secret", "Secrets Management", "Show the value of the given secret in the secret store", 2 do
  group = ARGV[1]
  name = ARGV[2]
  puts credentials.get(group, name)
end

add_command_under_category "set-secret", "Secrets Management", "Set or change secret NAME of GROUP", 2 do
  with_restart = ARGV.delete("--with-restart")
  group = ARGV[1]
  name = ARGV[2]

  unless is_known_credential(group, name)
    msg = "chef-server-ctl set-secret: Unknown credential: '#{name}' (group '#{group}')"
    STDERR.puts msg
    raise SystemExit.new(1, msg)
  end

  env_name = "#{group.upcase}_#{name.upcase}"
  disp_name = "#{group} #{name}"
  password = capture_secret_value(env_name, disp_name, ARGV[3])
  set_secret_(group, name, password, with_restart)
end

add_command_under_category "remove-secret", "Secrets Management", "Remove secret NAME of GROUP", 2 do
  group = ARGV[1]
  name = ARGV[2]

  confirm_continue!("WARN: Removing a secret may render your chef-server inoperable.  Are you sure?")
  credentials.remove(group, name)
  credentials.save
end

def is_known_credential(group, name)
  return false unless KNOWN_CREDENTIALS[group]

  KNOWN_CREDENTIALS[group].member? name
end

def confirm_continue!(message)
  require "highline"
  return if ARGV.delete("--yes")

  STDERR.puts message
  unless HighLine.agree("Would you like to continue (y/n)? ")
    exit(0)
  end
end

def set_secret_(group, key, secret, with_restart = nil)
  credentials.add(group, key, value: secret, frozen: true, force: true)
  credentials.save

  lookup = "#{group}.#{key}"
  affected_services = Array(SERVICES_REQUIRING_RESTART[lookup]).select { |s| manage_or_other_service_enabled?(s) }

  return unless affected_services.any?

  service_list = affected_services.sort.join(", ")
  if with_restart
    puts "Restarting these services: #{service_list}"
    affected_services.each { |service| restart_manage_or_other_service(service) }
  else
    puts "Please restart these services: #{service_list}"
  end
end

def manage_or_other_service_enabled?(service)
  if service == "chef-manage"
    File.exist?(MANAGE_SVDIR)
  else
    service_enabled?(service)
  end
end

def restart_manage_or_other_service(service)
  if service == "chef-manage"
    Mixlib::ShellOut.new("chef-manage-ctl restart", env: { "SVDIR" => MANAGE_SVDIR }).run_command
  else
    run_sv_command_for_service("restart", service)
  end
end

def capture_secret_value(env_key, prompt = "secret", password_arg = nil)
  if password_arg
    password_arg
  elsif ENV[env_key]
    puts "Using value of environment variable #{env_key}. To interactively enter the password, unset this environment variable."
    ENV[env_key]
  else
    pass1 = HighLine.ask("Enter #{prompt}: " ) { |q| q.echo = false }
    pass2 = HighLine.ask("Re-enter #{prompt}: " ) { |q| q.echo = false }
    if pass1 == pass2
      pass2
    else
      STDERR.puts "ERROR: Passwords don't match"
      exit(1)
    end
  end
end
