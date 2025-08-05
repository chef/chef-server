-module(chef_license).

-export([get_license/0]).

get_license() ->
    chef_license_worker:get_license().
