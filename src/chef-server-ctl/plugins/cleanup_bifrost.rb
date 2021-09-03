require "pg"

CREATE_SQL = <<SQL
BEGIN;
CREATE UNLOGGED TABLE IF NOT EXISTS cleanup_tracking_auth_actors(
    authz_id  CHAR(32)
);

CREATE OR REPLACE RULE cleanup_auth_actor_creation_tracking AS ON INSERT TO auth_actor DO INSERT INTO cleanup_tracking_auth_actors VALUES (NEW.authz_id);
COMMIT;
SQL

CREATE_SQL2 = <<SQL
CREATE UNLOGGED TABLE IF NOT EXISTS cleanup_known_auth_actors(
    authz_id  CHAR(32)
);
SQL

DELETE_SQL = <<SQL
BEGIN;
DROP RULE IF EXISTS cleanup_auth_actor_creation_tracking ON auth_actor;
DROP TABLE IF EXISTS cleanup_tracking_auth_actors;
DROP TABLE IF EXISTS cleanup_known_auth_actors;
COMMIT;
SQL

CLEANUP_SQL = <<SQL
WITH good_auth_actors AS (
         SELECT authz_id
         FROM cleanup_tracking_auth_actors
              UNION
              SELECT authz_id FROM cleanup_known_auth_actors),
     orphaned_auth_actors AS (
         SELECT authz_id
         FROM auth_actor
         WHERE authz_id NOT IN (SELECT authz_id FROM good_auth_actors)
         LIMIT $1)
DELETE FROM auth_actor WHERE authz_id IN (SELECT authz_id FROM orphaned_auth_actors)
SQL

add_command_under_category "cleanup-bifrost", "cleanup", "Cleanup orphaned bifrost objects.", 2 do
  cleanup_args = ARGV[1..-1]
  options = {}

  OptionParser.new do |opts|
    opts.banner = "chef-server-ctl cleanup-bifrost [options]"
    opts.on("-b SIZE", "--batch-size SIZE", "How many authz actors to delete at a time") do |b|
      options[:batch_size] = b.to_i
    end

    opts.on("-w SECONDS", "--wait-time SECONDS", "How many seconds to wait before stating scan (default: 50)") do |w|
      options[:wait_time] = w.to_i
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
  # The wait_time is a safety-timer we use to avoid this race.  The
  # default of 50 seconds is:
  #
  #   (# of db/upstream calls after auth_actor creation * 5 seconds) * 2
  #
  if !options[:wait_time]
    options[:wait_time] = 50
  elsif options[:wait_time] <= 0
    puts "Invalid wait time: #{options[:wait_time]}"
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

  run_cleanup(bifrost_db, options[:batch_size], options[:wait_time])
end

def run_cleanup(bifrost_db, batch_size, wait_time)
  safety_check(bifrost_db)
  install_bifrost_tracking_table(bifrost_db)

  begin
    puts "Sleeping #{wait_time} seconds to account for in-flight requests not captured by tracking table"
    sleep wait_time
    estimated_deletion_count = print_and_return_estimate(known_actor_list, bifrost_db)

    if estimated_deletion_count <= 0
      puts "Estimated deletion count 0. Aborting"
      exit(0)
    end

    install_known_actor_table(known_actor_list, bifrost_db)
    run_bifrost_scan(batch_size, bifrost_db)
  ensure
    remove_bifrost_tracking_table(bifrost_db)
  end
end

def known_actor_list
  @known_actor_list ||= timed "Fetching initial opscode_chef clients and user list" do
    clients_and_users = erchef_db.exec("SELECT authz_id FROM clients UNION select authz_id FROM users")
    clients_and_users.map do |real_actor|
      real_actor["authz_id"]
    end
  end
end

def safety_check(db)
  res = db.exec("SELECT * FROM pg_tables
                          WHERE tablename='cleanup_tracking_auth_actors'")
  if res.ntuples > 0
    puts "ERROR: cleanup_tracking_auth_actors already exists.  cleanup-bifrost may be running."
    puts "ERROR: If you are sure cleanup-bifrost is not running, you can clean up the tracking tables with: chef-server-ctl cleanup-bifrost --force-cleanup"
    exit(1)
  end
end

def install_bifrost_tracking_table(db)
  timed "Installing tracking tables into bifrost database" do
    db.exec(CREATE_SQL)
  end
end

def install_known_actor_table(list, db)
  timed "Populating known actor table" do
    db.exec(CREATE_SQL2)
    db.copy_data("COPY cleanup_known_auth_actors FROM STDIN") do
      list.each do |id|
        db.put_copy_data(id.concat("\n"))
      end
    end
  end
end

def remove_bifrost_tracking_table(db)
  timed "Removing tracking tables from bifrost database" do
    db.exec(DELETE_SQL)
  end
end

def fetch_auth_actor_count(db)
  timed "Fetching count from bifrost auth_actor" do
    db.exec("SELECT count(*) FROM auth_actor").first["count"].to_i
  end
end

def print_and_return_estimate(known_actor_list, db)
  tcount = fetch_auth_actor_count(db)
  estimated_del_count = [0, tcount - known_actor_list.length].max
  puts "\n----------------------------------------"
  puts " Total chef users+clients: #{known_actor_list.length}"
  puts "Total bifrost auth_actors: #{tcount}"
  puts "Deletion Candidates (est): #{estimated_del_count}"
  puts "----------------------------------------\n"

  estimated_del_count
end

def run_bifrost_scan(batch_size, db)
  total_deleted = 0
  loop do
    deletion_count = timed "Processing batch of #{batch_size} unknown auth_actors. " do
      count = db.exec(CLEANUP_SQL, [batch_size]).cmd_tuples
      printf "Deleted #{count} actor#{count == 1 ? "" : "s"} (total = #{total_deleted + count})"
      count
    end

    total_deleted += deletion_count
    break if deletion_count == 0
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

def erchef_db
  @erchef_db ||= ::PG::Connection.open(::ChefServerCtl::Config.erchef_sql_connuri)
end

def bifrost_db
  @bifrost_db ||= ::PG::Connection.open(::ChefServerCtl::Config.bifrost_sql_connuri)
end
