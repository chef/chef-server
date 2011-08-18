%% version used in X-CHEF-VERSION header sent to server
-define(CHEF_VERSION, "0.10.0").

-record(req_config, {
          api_root :: string(),
          name :: string(),
          private_key :: term()}).
