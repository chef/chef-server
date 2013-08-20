# NOTE:
#
# Uses the value of node['private_chef']['postgresql']['username'] as
# the user to run the user-creation psql command

def whyrun_supported?
  true
end

use_inline_resources

action :create do
  execute "create_postgres_user_#{new_resource.username}" do
    command "psql --dbname template1 --command \"#{create_user_query}\""
    user node['private_chef']['postgresql']['username']
    not_if {user_exist?}
    retries 30
  end
end

def create_user_query
  q = ["CREATE USER #{new_resource.username} WITH"]
  q << "SUPERUSER" if new_resource.superuser
  q << "ENCRYPTED PASSWORD '#{new_resource.password}'"
  q << ";"
  q.join(" ")
end

def user_exist?
  command = <<-EOM.gsub(/\s+/," ").strip!
    psql --dbname template1
         --tuples-only
         --command "SELECT rolname FROM pg_roles WHERE rolname='#{new_resource.username}';"
    | grep #{new_resource.username}
  EOM

  s = Mixlib::ShellOut.new(command,
                           :user => node['private_chef']['postgresql']['username'])
  s.run_command
  s.exitstatus == 0
end
