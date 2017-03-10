add_command_under_category "set-ldap-bind-password", "Secrets Management", "Add or change LDAP bind password", 2 do
  ARGV.delete("--yes") # remove --yes so users can give it in all cases in automation
  bind_password = get_secret("BIND_PASSWORD", "LDAP bind password")
  set_secret("ldap", "bind_password", bind_password)
end

add_command_under_category "set-db-superuser-password", "Secrets Management", "Add or change DB superuser password", 2 do

  confirm_continue!("WARN: Manually setting the DB superuser password is only supported for external postgresql instances")
  password = get_secret("DB_PASSWORD", "DB superuser password")
  set_secret("postgresql", "db_superuser_password", password)
end

add_command_under_category "set-actions-password", "Secrets Management", "Add or change the rabbitmq actions queue password", 2 do
  confirm_continue!("WARN: Manually setting the actions password is only supported for external rabbitmq instances")
  password = get_secret("ACTIONS_PASSWORD", "actions queue password")
  set_secret("rabbitmq", "actions_password", password)
end

add_command_under_category "set-data-collector-token", "Secrets Management", "Set or change the data collector token", 2 do
  password = get_secret("DATA_COLLECTOR_TOKEN", "the data collector token provided by Automate")
  set_secret("data_collector", "token", password)
end

def confirm_continue!(message)
  require 'highline'
  return if ARGV.delete("--yes")

  STDERR.puts message
  if !HighLine.agree("Would you like to continue (y/n)? ")
    exit(0)
  end
end

def set_secret(group, key, secret)
  # TODO(ssd) 2017-03-07: We could just use veil directly here since we already require it other places
  # in the -ctl commands...
  require 'mixlib/shellout'
  cmd = Mixlib::ShellOut.new("/opt/opscode/embedded/bin/veil-ingest-secret #{group}.#{key}", input: secret)
  cmd.run_command
end

def get_secret(env_key, prompt='secret')
  password_arg = ARGV[3]
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
