# NOTE:
#
# Uses the value of node['private_chef']['postgresql']['username'] as
# the user to run the user-creation psql command

def whyrun_supported?
  true
end

action :create do
  EcPostgres.with_connection(node) do |connection|

    user_info = connection.exec('select usesuper, passwd from pg_shadow where usename = $1', [ new_resource.username ])
    if user_info.ntuples > 0
      user_info = user_info[0]
      changes = [ "Update Postgres user #{new_resource.username}" ]
      sql = ''
      if user_info['usesuper'] != (new_resource.superuser ? 't' : 'f')
        changes << "  Set superuser to #{!!new_resource.superuser}"
        sql << (new_resource.superuser ? ' SUPERUSER' : ' NOSUPERUSER')
      end
      if new_resource.password && user_info['passwd'] != ::PGconn.encrypt_password(new_resource.password, new_resource.username)
        changes << '  Update password'
        sql << " ENCRYPTED PASSWORD '#{connection.escape(new_resource.password)}'"
      end
      if changes.size > 1
        converge_by changes do
          connection.exec("ALTER USER #{new_resource.username}#{sql}")
        end
      end
    else
      changes = [ "Create Postgres user #{new_resource.username}" ]
      sql = ''
      if new_resource.superuser
        changes << "  Set superuser to #{!!new_resource.superuser}"
        sql << (new_resource.superuser ? ' SUPERUSER' : ' NOSUPERUSER')
      end
      if new_resource.password
        changes << '  Update password'
        sql << " ENCRYPTED PASSWORD '#{connection.escape(new_resource.password)}'"
      end
      converge_by changes do
        connection.exec("CREATE USER #{new_resource.username}#{sql}")
      end
    end
  end
end
