-module(itest_util).

-compile([export_all, nowarn_export_all]).

-include_lib("eunit/include/eunit.hrl").
-include("chef_db.hrl").
-include("chef_types.hrl").

make_id(Prefix) when is_binary(Prefix) ->
    case size(Prefix) of
        Size when Size > 32 ->
            error(prefix_too_long_for_id);
        Size when Size =:= 32 ->
              Prefix;
          Size ->
            iolist_to_binary([Prefix, lists:duplicate(32 - Size, $0)])
    end.

make_az_id(Prefix) ->
    make_id(<<"a11", Prefix/binary>>).

actor_id() ->
    make_az_id(<<"ffff">>).

the_org_id() ->
    make_id(<<"aa1">>).

other_org_id() ->
    make_id(<<"bb2">>).

set_env(App, Config) ->
    [ok = application:set_env(App, Key, Value) || {Key, Value} <- Config ].

create_record(Record) ->
    chef_sql:create_object(chef_object:create_query(Record), Record).

fetch_record(Record) ->
    chef_sql:fetch_object(
      chef_object:fields_for_fetch(Record),
      element(1, Record),
      chef_object:find_query(Record),
      chef_object:record_fields(Record)
          ).

update_record(Record) ->
  chef_sql:do_update(chef_object:update_query(Record), chef_object:fields_for_update(Record)).

delete_record(Record) ->
    chef_sql:delete_object(chef_object:delete_query(Record), chef_object:id(Record)).

list_records(Record) ->
    chef_sql:fetch_object_names(Record).
