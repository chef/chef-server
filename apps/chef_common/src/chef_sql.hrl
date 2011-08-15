%% a bit odd, but field names have to match column names for helper
%% function to work.
-record(chef_user, {'id',
                    'authz_id',
                    'username',
                    'pubkey_version',
                    'public_key'}).

