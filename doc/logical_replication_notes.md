- stop services on source (except pg)
- add logical slot on source, w
- stop services on target (except pg) ,

Assumptions for now:

chef pgsql superuser and password matches.

But what do I need to do to prove it? ThinkPad T460p

  - bring up a database
  - populate it with something
  - pgbasebackup it to a new place.
  - start it new from that place (diff port for now)
  x wal_level logical
  - start an erlang-side recieve - for now, we'll just wrap
    the receiver that ships with a port, and parse it line by line.
Some light reading: https://www.postgresql.org/docs/9.5/static/logicaldecoding-output-plugin.html

Source server:
  - postgres
  - test
Sequence of run:
- run command to enable rep receiver which will:
  - stop services except pg
  - add extension
  - with superuser (or add replication perm to other): pgbasebackup against old server (which must have repl enabled)
  - start services, with migrator app enabled as a receiver.
  - migrator subscripts to updates and handlees them.

- run command to enable replication which will
   - stop local chef server
   - add physical slot
   - add logical slot
   - start local chef server
NOTE: needs to be logical per DB

So what if we always listen from the old system? Upstream/downstream
connections change, but that means.

Frankly - it may not matter. We must have
opscode_chef=# SELECT * FROM pg_create_logical_replication_slot('regression_slot', 'test_decoding');
    slot_name    | xlog_position
-----------------+---------------
 regression_slot | 0/1B624B0

 pg_hba - added replicatoin entry for opscode-pgsql
NOTES

% This is the query we'd run (need to look closer at available options too)
% and it woudl return :
%
 SELECT * FROM pg_logical_slot_get_changes('regression_slot', NULL, NULL, 'include-xids', '0');

 -> location
 -> xid -> txid
 -> __ -> string representation of change
% But it's faster to add a port for now - we just need to
%%====================================================================
%% Internal functions
%%====================================================================
%%
%% Format example:
%% BEGIN 1234
% $able public.org_user_associations: INSERT: org_id[character]:'80ad4e47a4c182da9fc5e1c900c65146' user_id[character varying]:'0000000000001109bcd5833ee8678c4f' last_updated_by[character]:'29e307e4c35a767cca1b4fe4d65fcad0' created_at[timestamp without time zone]:'2017-05-18 22:32:13' updated_at[timestamp without time zone]:'2017-05-18 22:32:13'
% COMMIT 1234


 input = "table public.org_user_associations:
          INSERT:
          org_id[character]:'80ad4e47a4c182da9fc5e1c900c65146'
          user_id[character varying]:'0000000000001109bcd5833ee8678c4f'
          last_updated_by[character]:'29e307e4c35a767cca1b4fe4d65fcad0'
          created_at[timestamp without time zone]:'2017-05-18 22:32:13' updated_at[timestamp without time zone]:'2017-05-18 22:32:13'"


- wal level must be logical which it *seems* contains hot_standby + some, so no risk for chef-backend?
- interesting note: Amaon's DMS is built on top of the test_decode output plugin

   ```
   #
    def basebackup(new_leader, opts = {})
      opts = with_default_recovery_options(opts)
      status = nil

      Dir.mktmpdir('chef-backend') do |dir|
        if opts[:try_restore_config]
          LibCB::Log.debug "Attempting to move existing configuration to #{dir}"
          move_config(dir)
        end

        begin
          # pg_basebackup doesn't work with existing data on disk
          remove_data_dir
          status = run_pg_basebackup(new_leader, opts)
        ensure
          if opts[:try_restore_config]
            LibCB::Log.debug "Attempting to restore existing configuration from #{dir}"
            restore_config(dir)
          end
        end
      end
      status.success?
    end


    def as_pg_user(cmd)
      if Process.euid == 0
        run_command("su #{@pg_user} -c '#{cmd}'")
      else
        LibCB::Log.debug("Not running as root (uid = #{Process.euid})")
        run_command(cmd)
      end
    end

    def run_pg_ctl(cmd)
      as_pg_user("#{@pg_ctl} -D #{@data_dir} #{cmd}")
    end
    def run_pg_basebackup(new_leader, opts)
      # pg_basebackup only connects to the *remote* server. No local
      # database exists by the time we get here.
      user = opts[:user]
      port = opts[:port]
      pass = opts[:pass]g
      ENV['PGPASSWORD'] = pass

      cmd = "#{@pg_basebackup} -h #{new_leader} -p #{port} -U #{user} -D #{@data_dir} --xlog-method=stream -v -P"
      as_pg_user(cmd)
    end
  def remove_data_dir
      FileUtils.rm_rf(@data_dir, secure: true)
      FileUtils.mkdir(@data_dir, :mode => 0700)
      FileUtils.chown(@pg_user, @pg_group, @data_dir)
    end

    def move_config(dir)
      conf_path = File.join(@data_dir, "postgresql.conf")
      hba_path = File.join(@data_dir, "pg_hba.conf")
      FileUtils.mv(conf_path, dir) if File.exist?(conf_path)
      FileUtils.mv(hba_path, dir) if File.exist?(hba_path)
    end

    def restore_config(dir)
      conf_path = File.join(dir, "postgresql.conf")
      hba_path = File.join(dir, "pg_hba.conf")
      FileUtils.mv(conf_path, @data_dir) if File.exist?(conf_path)
      FileUtils.mv(hba_path, @data_dir) if File.exist?(hba_path)
    end

    def move_certs(dir)
      cert_path = File.join(@data_dir, "server.crt")
      key_path = File.join(@data_dir, "server.key")
      FileUtils.mv(cert_path, dir) if File.exist?(cert_path)
      FileUtils.mv(key_path, dir) if File.exist?(key_path)
    end

    def restore_certs(dir)
      backup_cert_path = File.join(dir, "server.crt")
      backup_key_path = File.join(dir, "server.key")
      FileUtils.mv(backup_cert_path, @data_dir) if File.exist?(backup_cert_path)
      FileUtils.mv(backup_key_path, @data_dir) if File.exist?(backup_key_path)
    end
