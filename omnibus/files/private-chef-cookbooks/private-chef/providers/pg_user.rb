#
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# NOTE:
#
# Uses the value of node['private_chef']['postgresql']['username'] as
# the user to run the user-creation psql command

action :create do
  if new_resource.local_connection
    EcPostgres.with_local_connection(node) do |connection|
      do_create(connection)
    end
  else
    EcPostgres.with_connection(node) do |connection|
      do_create(connection)
    end
  end
end

def do_create(connection)
  user_info = connection.exec('select usesuper from pg_catalog.pg_user where usename = $1', [ new_resource.username ])
  if user_info.ntuples > 0
    update_user(connection, user_info[0])
  else
    create_user(connection)
  end
end

def update_user(connection, user_info)
  changes = [ "Update Postgres user #{new_resource.username}" ]
  sql = ''
  if user_info['usesuper'] != (new_resource.superuser ? 't' : 'f')
    changes << "  Set superuser to #{!!new_resource.superuser}"
    sql << (new_resource.superuser ? 'SUPERUSER' : 'NOSUPERUSER')
  end
  begin
    # In some configurations we may not have access to pg_shadow - let's try it so we can see if we need to
    # change a password. If access fails, we'll assume that we do need to update password (at worst, this
    # will be a no-op)
    pg_shadow_info = connection.exec('select passwd from pg_shadow where usename = $1', [ new_resource.username ])
    if pg_shadow_info.ntuples > 0
      pg_shadow_info = pg_shadow_info[0]
      if new_resource.password && pg_shadow_info['passwd'] != ::PGconn.encrypt_password(new_resource.password, new_resource.username)
        changes << '  Update password'
        sql << " ENCRYPTED PASSWORD '#{connection.escape(new_resource.password)}'"
      end
    end
  rescue PG::InsufficientPrivilege
    changes << '  Update password'
    sql << " ENCRYPTED PASSWORD '#{connection.escape(new_resource.password)}'"
  end
  if changes.size > 1
    converge_by changes do
      connection.exec("ALTER USER \"#{new_resource.username}\" #{sql}")
    end
  end
end

def create_user(connection)
  changes = [ "Create PostgreSQL user #{new_resource.username}" ]
  sql = ''
  if new_resource.superuser
    changes << "  Give superuser access to #{!!new_resource.superuser}"
    sql << (new_resource.superuser ? 'SUPERUSER' : 'NOSUPERUSER')
  end
  if new_resource.password
    changes << '  Set password'
    sql << " ENCRYPTED PASSWORD '#{connection.escape(new_resource.password)}'"
  end
  statements = [ "CREATE USER \"#{new_resource.username}\" #{sql} " ]
  # To support modifying databases owned by this user, the superuser must
  # have the new user's role.
  if node['private_chef']['postgresql']['external']
    statements << "GRANT #{new_resource.username} TO \"#{node['private_chef']['postgresql']['db_superuser']}\""
    changes << "  Grant role '#{new_resource.username}' to database superuser."
  end
  converge_by changes do
    statements.each do |statement|
      connection.exec(statement)
    end
  end
end
