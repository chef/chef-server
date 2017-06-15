add_command_under_category "set-db-superuser-password", "Secrets Management", "Add or change DB superuser password", 2 do
  confirm_continue!("WARN: Manually setting the DB superuser password is only supported for external postgresql instances")
  password = capture_secret_value("DB_PASSWORD", "DB superuser password", ARGV[3])
  set_secret_("postgresql", "db_superuser_password", password)
end

add_command_under_category "set-actions-password", "Secrets Management", "Add or change the rabbitmq actions queue password", 2 do
  confirm_continue!("WARN: Manually setting the actions password is only supported for external rabbitmq instances")
  password = capture_secret_value("ACTIONS_PASSWORD", "actions queue password", ARGV[3])
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
  "manage" => ["secret_key_base", "secret_token"],
  "saml" => ["client_id", "client_secret"],
  "push-jobs-server" => ["pushy_priv_key", "pushy_pub_key", "sql_password", "sql_ro_password"],
  "opscode-reporting" => ["rabbitmq_password", "sql_password", "sql_ro_password"],
}

#postgres/rabbitmq not in default.yml but pswds reset? why not in list?
#opscode-expander, redis_lb, nginx keys from run scripts
#ldap bind_password depend on ldap?
#some KNOWN_CREDENTIALS don't match with these secrets. Out of date?
FROM_DEFAULTS = {
 "chef-server.superuser_key"=>["oc_reporting", "oc-chef-pedant"],
 "chef-server.webui_pub_key"=>["oc_erchef", "oc_reporting"],
 "bookshelf.access_key_id"=>["oc_erchef", "bookshelf"],
 "bookshelf.secret_access_key"=>["oc_erchef", "bookshelf"],
 "bookshelf.sql_password"=>["bookshelf"],
 "data_collector.token"=>["oc_erchef", "nginx"],
 "rabbitmq.actions_password"=>["oc_erchef", "oc_reporting"],
 "rabbitmq.management_password"=>["oc_erchef"],
 "rabbitmq.password"=>["oc_erchef", "opscode-expander"],
 "oc_bifrost.superuser_id"=>["oc_erchef", "oc_bifrost", "chef-mover"],
 "ldap.bind_password"=>["oc_erchef"],
 "chef-server.webui_key"=>["oc-id", "oc-chef-pedant"],
 "oc_id.sql_password"=>["oc-id"],
 "oc_id.secret_key_base"=>["oc-id"],
 "oc_bifrost.sql_password"=>["oc_bifrost"],
 "opscode_erchef.sql_password"=>["oc_erchef", "chef-mover"],
 "opscode-reporting.sql_password"=>["oc_reporting"],
 "redis_lb.password"=>["chef-mover", "nginx", "redis_lb"],
}

FROM_KNOWN_CREDENTIALS = {
  ["ldap", "bind_password"] => "ldap",
  ["data_collector", "token"] => "data_collector",
  ["rabbitmq", "password"] => "rabbitmq",
  ["rabbitmq", "management_password"] => "rabbitmq",
  ["redis_lb", "password"] => "redis_lb",
  ["drbd", "shared_secret"] => "drbd",
  ["keepalived", "vrrp_instance_password"] => "keepalived",
  ["opscode_erchef", "sql_password"] => "opscode_erchef",
  ["opscode_erchef", "sql_ro_password"] => "opscode_erchef",
  ["oc_bifrost", "superuser_id"] => "oc_bifrost",
  ["oc_bifrost", "sql_password"] => "oc_bifrost",
  ["oc_bifrost", "sql_ro_password"] => "oc_bifrost",
  ["oc_id", "secret_key_base"] => "oc_id",
  ["oc_id", "sql_password"] => "oc_id",
  ["oc_id", "sql_ro_password"] => "oc_id",
  ["bookshelf", "access_key_id"] => "bookshelf",
  ["bookshelf", "secret_access_key"] => "bookshelf",
  ["bookshelf", "sql_password"] => "bookshelf",
  ["bookshelf", "sql_ro_password"] => "bookshelf",
  ["manage", "secret_key_base"] => "manage",
  ["manage", "secret_token"] => "manage",
  ["saml", "client_id"] => "saml",
  ["saml", "client_secret"] => "saml",
  ["push-jobs-server", "pushy_priv_key"] => "push-jobs-server",
  ["push-jobs-server", "pushy_pub_key"] => "push-jobs-server",
  ["push-jobs-server", "sql_password"] => "push-jobs-server",
  ["push-jobs-server", "sql_ro_password"] => "push-jobs-server",
  ["opscode-reporting", "rabbitmq_password"] => "opscode-reporting",
  ["opscode-reporting", "sql_password"] => "opscode-reporting",
  ["opscode-reporting", "sql_ro_password"] => "opscode-reporting"
}

add_command_under_category "show-secret", "Secrets Management", "Show the value of the given secret in the secret store", 2 do
  group = ARGV[3]
  name = ARGV[4]
  puts credentials.get(group, name)
end

add_command_under_category "set-secret", "Secrets Management", "Set or change secret NAME of GROUP", 2 do
  with_restart = ARGV.delete("--with-restart")
  group = ARGV[3]
  name = ARGV[4]

  unless is_known_credential(group, name)
    msg = "chef-server-ctl set-secret: Unknown credential: '#{name}' (group '#{group}')"
    STDERR.puts msg
    raise SystemExit.new(1, msg)
  end

  env_name = "#{group.upcase}_#{name.upcase}"
  disp_name = "#{group} #{name}"
  password = capture_secret_value(env_name, disp_name, ARGV[5])
  set_secret_(group, name, password, with_restart)
end

add_command_under_category "remove-secret", "Secrets Management", "Remove secret NAME of GROUP", 2 do
  group = ARGV[3]
  name = ARGV[4]

  confirm_continue!("WARN: Removing a secret may render your chef-server inoperable.  Are you sure?")
  credentials.remove(group, name)
  credentials.save
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

def set_secret_(group, key, secret, with_restart=nil)
  credentials.add(group, key, value: secret, frozen: true, force: true)
  credentials.save
  lookup = "#{group}.#{key}"
  #TODO: do we need to guard against services being empty? (KNOWN_CREDS being out of sync w/ service list)
  services = FROM_DEFAULTS[lookup]
  if with_restart
    services.each { |service| run_sv_command("restart", service) }
  else
    service_list = services.sort.join(", ")
    puts "You have changed #{key} for #{group}. Please restart these necessary services: #{service_list}"
  end
end

def capture_secret_value(env_key, prompt='secret', password_arg = nil)
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
