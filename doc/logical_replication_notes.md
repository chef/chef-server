=== Note ===
This notes file isn't really markdown,a nd it's more a collection
of notes and thoughts and potentially useful code snippets.

% Open Things
% Q is it safe to asssume that any secondary TX in the
  same commit is trigger-based and not something we should apply?

% R enabling sync mode should require a version check to compare target/dest
%   end ensure they match

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


# More Raw Crap

 %{<<"0/37D9F48">>,<<"23359">>,<<"BEGIN 23359">>},
 %{<<"0/37D9F48">>,<<"23359">>,
  %<<"table public.clients: INSERT: id[character varying]:'da11fd5c59ad7cd575c4d53f6836168a' org_id[character]:'b22a18ce74e549b0ccb7da11fd5c59ad' authz_id[character]:'c3250a4fc08c482076abbaf6ed0b0ead' name[text]:'pedant_admin_client_api_23519' public_key[text]:'-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApa0mNQI1MT7z98otsn1B\noMmgSq9Xl/pB5uHcQ28HtiAxGnzuD47ZLct091l8DdfOz+cB1hPLkQxBM9Yyz7Cw\nGgeU8HKF+2L+KEDvnukI3Nk+O8b5Ghf+JVGStnNAmGbIPfNU5sTvadJIQ1VjpZkx\nQYCSTNM3DdsU0JeX9YNXCBhFn/pkjXVeXLkYAdU2JU1V6L+mutNYNnWpP9pqA7Yu\ncoyQjVwRSll1P81wFNZw9Y3nMztknHy5+4ZUoOP9yf1Bom/7e+sShCmDaMZeUbvq\nBnCVB3NGye8mRQDzMoJlhCemtHCBJpUl+Waz12N0DmWY+Mrrskqsx8mnMhhX797g\njwIDAQAB\n-----END PUBLIC KEY-----\n\n' validator[boolean]:false last_updated_by[character]:'f1cd13cb541436c290c2b4eb3685f6c2' created_at[timestamp without time zone]:'2017-06-05 18:06:47' updated_at[timestamp without time zone]:'2017-06-05 18:06:47' pubkey_version[smallint]:0 admin[boolean]:false">>},
 %{<<"0/37DA358">>,<<"23359">>,
  %<<"table public.keys: INSERT: id[character]:'da11fd5c59ad7cd575c4d53f6836168a' key_name[text]:'default' public_key[text]:'-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApa0mNQI1MT7z98otsn1B\noMmgSq9Xl/pB5uHcQ28HtiAxGnzuD47ZLct091l8DdfOz+cB1hPLkQxBM9Yyz7Cw\nGgeU8HKF+2L+KEDvnukI3Nk+O8b5Ghf+JVGStnNAmGbIPfNU5sTvadJIQ1VjpZkx\nQYCSTNM3DdsU0JeX9YNXCBhFn/pkjXVeXLkYAdU2JU1V6L+mutNYNnWpP9pqA7Yu\ncoyQjVwRSll1P81wFNZw9Y3nMztknHy5+4ZUoOP9yf1Bom/7e+sShCmDaMZeUbvq\nBnCVB3NGye8mRQDzMoJlhCemtHCBJpUl+Waz12N0DmWY+Mrrskqsx8mnMhhX797g\njwIDAQAB\n-----END PUBLIC KEY-----\n\n' key_version[integer]:0 created_at[timestamp without time zone]:'2017-06-05 18:06:47.677758' expires_at[timestamp without time zone]:'infinity' last_updated_by[character]:'                                ' updated_at[timestamp without time zone]:'2017-06-05 18:06:47.677758'">>},
 %{<<"0/37DA660">>,<<"23359">>,<<"COMMIT 23359">>},
  %%% Here it is in erlang form.  I believe this is something we manually inserted
 %%%  (2nd record) and was not triggered, but this does raise the question of what to do
 %%%  with values that are auto-inserted by trigger such as in bifrost.
 %%%  TODO We might need to disable triggers that insert/update data when we are in sync mode.
  %%%
 %{<<"0/37DF8C0">>,<<"23374">>,<<"BEGIN 23374">>},
 %{<<"0/37DF8C0">>,<<"23374">>,
 % <<"table public.users: INSERT: id[character]:'00000000000022a699c6688803182a65' authz_id[character]:'8549d50646c4e3ea58bd8df8e5befda8' username[text]:'pedant_admin_user_api_23519' email[text]:'pedant_admin_user_api_23519@chef.io' pubkey_version[integer]:0 public_key[text]:'-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA6yZzj04ZjqnUVPpibb8i\n1mzLwgLJak/+3O5EYEaWgDJVrR4tGqt1eKtyjaFsFBBoDEnEl5zGDdqvxUPLaLbO\nASOLwRLt2SsPRq1iQNFOXz6TFkbcGQv9yPKg3sN/3UXKcIylTEjU7bkGJK7V9Xxj\nrveTNEnqmKEHkpHwLGV+AljygxiriEF2v9+N3enl8Ts0WwZ3TzXhHlcP2SCkCkeC\nRZwztapez/5R8qQdtcFkqRWaZHy5ErY3lWe8sv8UDxCEqsS+YbVMY2NIy4e6zPTn\nau3Tc51oELeiPkj0Wtiz7UOBbrjdB++/0kaaUJTEV/Uh5mC3DDJtuy5DjHqJ9+iU\nzQIDAQAB\n-----END PUBLIC KEY-----\n\n' serialized_object[text]:'{\"display_name\":\"pedant_admin_user_api_23519\",\"first_name\":\"pedant_admin_user_api_23519\",\"last_name\":\"pedant_admin_user_api_23519\"}' last_updated_by[character]:'c3fbae8bae3f5677e319a8282bc180f2' created_at[timestamp without time zone]:'2017-06-05 18:06:48' updated_at[timestamp without time zone]:'2017-06-05 18:06:48' external_authentication_uid[text]:null recovery_authentication_enabled[boolean]:false admin[boolean]:false hashed_password[text]:'$2a$12$1pw5OrdjFhVlYMP57YOq4ODDy2s83wiiuXcHlE2Ix1uMHEYr8C9O2' salt[text]:'$2a$12$1pw5OrdjFhVlYMP57YOq4O' hash_type[password_hash_type]:'bcrypt'">>},
 %{<<"0/37DFDE8">>,<<"23374">>,
 % <<"table public.keys: INSERT: id[character]:'00000000000022a699c6688803182a65' key_name[text]:'default' public_key[text]:'-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA6yZzj04ZjqnUVPpibb8i\n1mzLwgLJak/+3O5EYEaWgDJVrR4tGqt1eKtyjaFsFBBoDEnEl5zGDdqvxUPLaLbO\nASOLwRLt2SsPRq1iQNFOXz6TFkbcGQv9yPKg3sN/3UXKcIylTEjU7bkGJK7V9Xxj\nrveTNEnqmKEHkpHwLGV+AljygxiriEF2v9+N3enl8Ts0WwZ3TzXhHlcP2SCkCkeC\nRZwztapez/5R8qQdtcFkqRWaZHy5ErY3lWe8sv8UDxCEqsS+YbVMY2NIy4e6zPTn\nau3Tc51oELeiPkj0Wtiz7UOBbrjdB++/0kaaUJTEV/Uh5mC3DDJtuy5DjHqJ9+iU\nzQIDAQAB\n-----END PUBLIC KEY-----\n\n' key_version[integer]:0 created_at[timestamp without time zone]:'2017-06-05 18:06:48.474687' expires_at[timestamp without time zone]:'infinity' last_updated_by[character]:'                                ' updated_at[timestamp without time zone]:'2017-06-05 18:06:48.474687'">>},
 %{<<"0/37E0108">>,<<"23374">>,<<"COMMIT 23374">>},

%
% We're going to do this brute-force-ways for now, because we have a reasonable
% number of tables to manage.
%
%
%
  % The pattern we're looki
  % $tablename: $fieldname1[$datatype1]:'$value1' $fieldname2[$datatype2]:'$value2' $fieldnamen[$datatypen]:'$valuen'
  % users: INSERT: id[character]:'0000000000009cd8a4dcbba29e15b0c0' authz_id[character]:'f5477ed38d0fae9f9547dea9f40a24f7' username[text]:'pedant_testorg_api_23439_owner' email[text]:'pedant_testorg_api_23439_owner@chef.io' pubkey_version[integer]:0 public_key[text]:'-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA17bbwaecnsRq44XYvknR\nUGS2PSoz4zmJMYC7vyrijHxTyY7m6q8vzeUh7mL38o3FqkcqoffQ1Vfu0THtlqsv\nbyW/nqbV0OGSmX22YPa4Fjg7aLbQ2++chxG9NBzQGrQNn54+DNipMWNSZyAU6bbd\nXgXnmBTxiaqIpLZSJbZB0NKZYTpRd1GYrJAYmquYs0s+WCdTRH6Ebp1NgIWjpVis\n8I7TE42dcndCp5DcN1xP4MQV6YiqM0Ss7Tmv+XcUsgOkXUTJocsi6OI8d3c+QFWh\nUBZEXxiBtKVN5YS1+IeaQBsuNBIZi2qTEcgIQQxvFnIGjGYU5qrP3jzq6Q1EEYWe\nhwIDAQAB\n-----END PUBLIC KEY-----\n\n' serialized_object[text]:'{\"display_name\":\"pedant_testorg_api_23439_owner\",\"first_name\":\"pedant_testorg_api_23439_owner\",\"last_name\":\"pedant_testorg_api_23439_owner\"}' last_updated_by[character]:'c3fbae8bae3f5677e319a8282bc180f2' created_at[timestamp without time zone]:'2017-06-05 17:16:07' updated_at[timestamp without time zone]:'2017-06-05 17:16:07' external_authentication_uid[text]:null recovery_authentication_enabled[boolean]:false admin[boolean]:false hashed_password[text]:'$2a$12$Mzx1MLByrorRdHFDebOohebyAvtapLvg/h7ZOStu/u8/.jrsEhlaK' salt[text]:'$2a$12$Mzx1MLByrorRdHFDebOohe' hash_type[password_hash_type]:'bcrypt'">>


