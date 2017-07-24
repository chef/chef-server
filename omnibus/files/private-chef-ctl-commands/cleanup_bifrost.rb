require 'pg'

CREATE_SQL =<<SQL
BEGIN;
CREATE TABLE IF NOT EXISTS cleanup_tracking_auth_actors(
    authz_id  CHAR(32)
);

CREATE OR REPLACE RULE cleanup_auth_actor_creation_tracking AS ON INSERT TO auth_actor DO INSERT INTO cleanup_tracking_auth_actors VALUES (NEW.authz_id);
COMMIT;
SQL

DELETE_SQL =<<SQL
BEGIN;
DROP RULE IF EXISTS cleanup_auth_actor_creation_tracking ON auth_actor;
DROP TABLE IF EXISTS cleanup_tracking_auth_actors;
COMMIT;
SQL

add_command_under_category "cleanup-bifrost", "cleanup", "Cleanup orphaned bifrost objects.", 2 do
  cleanup_args = ARGV[3..-1]
  options = {}

  OptionParser.new do |opts|
    opts.banner = "chef-server-ctl cleanup-bifrost [options]"
    opts.on("-b SIZE", "--batch-size", "How many authz actors to delete at a time") do |b|
      options[:batch_size] = b.to_i
    end

    opts.on("--force-cleanup", "Clean up tracking tables (does not scan bifrost actors)") do |b|
      options[:cleanup_only] = true
    end

    opts.on("--estimate-only", "Print estimate of orphaned authz objects") do |b|
      options[:estimate_only] = true
    end
  end.parse!(cleanup_args)

  if !options[:batch_size]
    options[:batch_size] = 10000
  elsif options[:batch_size] <= 0
    puts "Invalid batch size: #{options[:batch_size]}"
    exit(1)
  end

  if options[:cleanup_only]
    remove_bifrost_tracking_table(bifrost_db)
    exit(0)
  end

  if options[:estimate_only]
    print_and_return_estimate(known_actor_list, bifrost_db)
    exit(0)
  end

  run_cleanup(bifrost_db, options[:batch_size])
end

def run_cleanup(bifrost_db, batch_size)
  safety_check(bifrost_db)
  install_bifrost_tracking_table(bifrost_db)
  # NOTE(ssd) 2017-07-24:
  # For the most part, the tracking table manages in-flight requests for us.
  # However, it is still possible that the following sequence happens:
  #    - new authz_id in bifrost database
  #    - tracking table inserted
  #    - known_actor_list fetched
  #    - new record in oc-chef database
  #
  # In the this case, the authz_id won't be in our list of known
  # clients, won't be in our tracking table, but will show up in our
  # search for orphaned authz_ids.
  #
  # Thus, this sleep is a 1-time wait to avoid that scenario.
  #
  puts "One-time sleep to account for in-flight requests not captured by tracking table"
  sleep 25
  begin
    estimated_deletion_count = print_and_return_estimate(known_actor_list, bifrost_db)

    if estimated_deletion_count <= 0
      puts "Estimated deletion count 0. Aborting"
      exit(0)
    end

    run_bifrost_scan(known_actor_list, batch_size, bifrost_db)
  ensure
    remove_bifrost_tracking_table(bifrost_db)
  end
end

def known_actor_list
  @known_actor_list ||= timed "Fetching initial opscode_chef clients and user list" do
    clients_and_users = erchef_db.exec("SELECT authz_id FROM clients UNION select authz_id FROM users")
    clients_and_users.map do |real_actor|
      real_actor['authz_id']
    end
  end
end

def safety_check(db)
  res = db.exec("SELECT * FROM pg_tables
                          WHERE tablename='cleanup_tracking_auth_actors'")
  if res.ntuples > 0
    puts "ERROR: cleanup_tracking_auth_actors already exists.  cleanup-bifrost may be running."
    puts "ERROR: If you are sure cleanup-bifrost is not running, you can clean up the tracking tables with: chef-backend-ctl cleanup-bifrost --force-cleanup"
  end
end

def install_bifrost_tracking_table(db)
  puts "Installing tracking tables into bifrost database"
  db.exec(CREATE_SQL)
end

def remove_bifrost_tracking_table(db)
  puts "Removing tracking tables into bifrost database"
  db.exec(DELETE_SQL)
end

def print_and_return_estimate(known_actor_list, db)
  tcount = timed "Fetchings all actor counts" do
    db.exec_params("SELECT count(*) FROM auth_actor").first['count'].to_i
  end

  estimated_del_count = [0, tcount - known_actor_list.length].max
  puts "\n----------------------------------------"
  puts " Total chef users+clients: #{known_actor_list.length}"
  puts "Total bifrost auth_actors: #{tcount}"
  puts "Deletion Candidates (est): #{estimated_del_count}"
  puts "----------------------------------------\n"

  estimated_del_count
end

def run_bifrost_scan(known_actor_list, batch_size, db)
  total_deleted = 0
  known_actors_str = to_pg_list(known_actor_list)
  loop do
    puts "Processing batch of #{batch_size}"

    candidates = timed "    Retrieving batch from auth_actors" do
      db.exec("SELECT authz_id FROM auth_actor WHERE authz_id NOT IN (#{known_actors_str}) LIMIT $1",
              [batch_size])
        .map {|r| r['authz_id'] }
    end

    if candidates.length == 0
      puts "No more candidates for deletion."
      break
    end

    confirmed_candidates = timed "    Ignoring any recently created actors from candidates (n = #{candidates.length})" do
      sql = "SELECT authz_id FROM cleanup_tracking_auth_actors WHERE authz_id IN (#{to_pg_list(candidates)})"
      newly_created = db.exec(sql).map {|r| r['authz_id'] }
      ret = candidates - newly_created
      printf " (ignored: #{newly_created.length}) (remaining: #{ret.length})"
      ret
    end

    if confirmed_candidates.length == 0
      puts "All candidates in batch were newly created. Stopping"
      break
    end

    delete_res = timed "    Deleting batch of #{confirmed_candidates.length} unknown auth_actors" do
      db.exec("DELETE FROM auth_actor WHERE authz_id IN (#{to_pg_list(confirmed_candidates)})")
    end

    total_deleted += delete_res.cmd_tuples
    break if delete_res.cmd_tuples == 0
  end
  puts "Total auth_actors removed: #{total_deleted}"
end

def timed(description)
  printf description
  st = Time.now
  res = yield
  printf ": #{Time.now - st}\n"
  res
end

def to_pg_list(array)
  "'" + array.join("','") + "'"
end

# TODO(ssd) 2017-07-24: I know these functions need to use veil to get
# the passwords. Right now has been written to potentialy run against
# Chef Server 12.11.0
def erchef_db
  @erchef_db ||= begin
                   running_config = JSON.parse(File.read("/etc/opscode/chef-server-running.json"))
                   erchef_config = running_config['private_chef']['opscode-erchef']
                   pg_config = running_config['private_chef']['postgresql']
                   ::PGconn.open('user' => erchef_config['sql_user'],
                                 'host' => pg_config['vip'],
                                 'password' => erchef_config['sql_password'],
                                 'port' => pg_config['port'],
                                 'dbname' => 'opscode_chef')
                 end
end

def bifrost_db
  @bifrost_db ||= begin
                    running_config = JSON.parse(File.read("/etc/opscode/chef-server-running.json"))
                    pg_config = running_config['private_chef']['postgresql']
                    ::PGconn.open('user' => pg_config['db_superuser'],
                                  'host' => pg_config['vip'],
                                  'password' => pg_config['db_superuser_password'],
                                  'port' => pg_config['port'],
                                  'dbname' => 'bifrost')
                  end
end
