add_command_under_category "set-db-superuser-password", "Secrets Management", "Add or change DB superuser password", 2 do

  confirm_continue!("WARN: Manually setting the DB superuser password is only supported for external postgresql instances")
  password = get_secret("DB_PASSWORD", "DB superuser password", ARGV[3])
  set_secret_("postgresql", "db_superuser_password", password)
end

add_command_under_category "set-actions-password", "Secrets Management", "Add or change the rabbitmq actions queue password", 2 do
  confirm_continue!("WARN: Manually setting the actions password is only supported for external rabbitmq instances")
  password = get_secret("ACTIONS_PASSWORD", "actions queue password", ARGV[3])
  set_secret_("rabbitmq", "actions_password", password)
end

KNOWN_CREDENTIALS = {
  "ldap" => ["bind_password"],
  "data_collector" => ["token"],
  "rabbitmq" => ["password", "management_password"],
  "redis_lb" => ["password"],
  "drbd" => ["shared_secret"],
  "keepalived" => ["vrrp_instance_password"],
  "opscode_erchef" => ["sql_password", "sql_ro_password"],
  "oc_bifrost" => ["superuser_id", "sql_password", "sql_ro_password"],
  "oc_id" => ["secret_key_base", "sql_password", "sql_ro_password"],
  "bookshelf" => ["access_key_id", "secret_access_key", "sql_password", "sql_ro_password"],
}

add_command_under_category "set-secret", "Secrets Management", "Set or change secret NAME of GROUP", 2 do
  group = ARGV[3]
  name = ARGV[4]

  unless is_known_credential(group, name)
    msg = "chef-server-ctl set-secret: Unknown credential: '#{name}' (group '#{group}')"
    STDERR.puts msg
    raise SystemExit.new(1, msg)
  end

  env_name = "#{group.upcase}_#{name.upcase}"
  disp_name = "#{group} #{name}"
  password = get_secret(env_name, disp_name, ARGV[5])
  set_secret_(group, name, password)
end

def is_known_credential(group, name)
  return false unless KNOWN_CREDENTIALS[group]

  KNOWN_CREDENTIALS[group].member? name
end

def confirm_continue!(message)
  require 'highline'
  return if ARGV.delete("--yes")

  STDERR.puts message
  if !HighLine.agree("Would you like to continue (y/n)? ")
    exit(0)
  end
end

def set_secret_(group, key, secret)
  # TODO(ssd) 2017-03-07: We could just use veil directly here since we already require it other places
  # in the -ctl commands...
  require 'mixlib/shellout'
  cmd = Mixlib::ShellOut.new("/opt/opscode/embedded/bin/veil-ingest-secret #{group}.#{key}", input: secret)
  cmd.run_command
end

def get_secret(env_key, prompt='secret', password_arg = nil)
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
