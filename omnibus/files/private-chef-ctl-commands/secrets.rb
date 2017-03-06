add_command_under_category "set-ldap-bind-password", "Secrets Management", "Add or change LDAP bind password", 2 do
  ARGV.delete("--yes") # remove --yes so users can give it in all cases in automation
  bind_password = get_secret("BIND_PASSWORD", "LDAP bind password")
  set_secret("ldap", "bind_password", bind_password)
end

add_command_under_category "set-db-superuser-password", "Secrets Management", "Add or change DB superuser password", 2 do
  require 'highline'

  if !ARGV.delete("--yes")
    STDERR.puts "WARN: Manually setting the DB superuser password is only supported for external postgresql instances"
    if !HighLine.agree("Would you like to continue (y/n)? ")
      exit(0)
    end
  end

  password = get_secret("DB_PASSWORD", "DB superuser password")
  set_secret("postgresql", "db_superuser_password", password)
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
    ENV[env_key]
  else
    prompt_for_password(prompt)
  end
end

def prompt_for_password(prompt)
  require 'highline'
  HighLine.ask("Enter #{prompt}: " ) { |q| q.echo = "*" }
end
