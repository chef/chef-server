%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%%
%% @author Marc A. Paradise <marc@chef.io>
%% Copyright 2016 Chef Software, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
-module(oc_chef_authz_acl_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").
safe_fetch_ids_test_() ->
    Subject = fun oc_chef_authz_acl:safe_fetch_ids/3,
    {foreach,
     fun() ->
             meck:new(oc_chef_authz_db),
             meck:expect(oc_chef_authz_db, authz_records_by_name,
                         fun valid_authz_records_by_name/3)
     end,
     fun(_) -> meck:unload(oc_chef_authz_db) end,
     [
      {"valid: the list of IDs are returned",
       ?_assertEqual([<<"id1">>,<<"id2">>],
                    Subject(client, <<"someorg">>, [<<"name1">>,<<"name2">>]))
      },
      {"invalid: an error is raised because a name is missing from the response",
       ?_assertThrow({invalid, user, [<<"name3">>]},
                    Subject(user, <<"someorg">>, [<<"name1">>,<<"name2">>, <<"name3">>]))
      }
     ]
    }.


fetch_actors_test_() ->
    Subject = fun oc_chef_authz_acl:fetch_actors/2,
    {foreach,
     fun() ->
             meck:new(oc_chef_authz_db)
     end,
     fun(_) -> meck:unload([oc_chef_authz_db]) end,
     [
      {"bad_actor: actors that have neither client nor user authz ids",
       fun() ->
               meck:expect(oc_chef_authz_db, find_org_actors_by_name,
                           fun invalid_actor_data_response/2),
               ?assertThrow({bad_actor, [<<"bob">>, <<"jane">>]},
                            Subject(<<"orgid">>, [<<"bob">>, <<"jane">>]))

       end},
      {"ambiguous_actor: actors that have both user and client authz ids",
       fun() ->
               meck:expect(oc_chef_authz_db, find_org_actors_by_name,
                           fun ambiguous_actor_data_response/2),
               ?assertThrow({ambiguous_actor, [<<"bob">>, <<"jane">>]},
                            Subject(<<"orgid">>, [<<"bob">>, <<"jane">>]))

       end},
      {"ok: actors that have only a client authz id",
       fun() ->
               meck:expect(oc_chef_authz_db, find_org_actors_by_name,
                           fun valid_client_data_response/2),
               FetchActorResponse = Subject(<<"any">>, [<<"bob">>, <<"jane">>]),
               ?assertEqual([<<"id-a">>, <<"id-a">>], FetchActorResponse)
       end},
      {"ok: actors that have only a user authz id",
       fun() ->
               meck:expect(oc_chef_authz_db, find_org_actors_by_name,
                           fun valid_user_data_response/2),
               FetchActorResponse = Subject(<<"any">>, [<<"bob">>, <<"jane">>]),
               ?assertEqual([<<"id-a">>, <<"id-a">>], FetchActorResponse)
       end}
     ]}.

validate_actors_clients_users_test_() ->
    Subject = fun oc_chef_authz_acl:validate_actors_clients_users/2,
    [
      {"an ACL which contains only groups, actors is valid",
       ?_assertEqual(ok,
                     Subject(<<"read">>, valid_actors_only_ej()))
      },
      {"an ACL which contains only groups, empty actors plus users and clients is valid",
       ?_assertEqual(ok,
                     Subject(<<"read">>, valid_actors_users_clients_ej()))
      },
      {"an ACL which contains groups, non-empty actors plus users and clients is not valid",
       ?_assertThrow(actors_must_be_empty,
                     Subject(<<"read">>, invalid_actors_users_clients_ej()))},
      {"an ACL which contains groups, actors,  plus clients is not valid",
       ?_assertThrow({one_requires_all, <<"clients">>, [<<"users">>]},
                     Subject(<<"read">>, invalid_clients_only_ej()))
      },
      {"an ACL which contains only groups, actors, plus users is not valid",
       ?_assertThrow({one_requires_all, <<"users">>, [<<"clients">>]},
                     Subject(<<"read">>, invalid_users_only_ej()))
      }
     ].

%%
%% Helpers for inputs and outputs.
%%

valid_user_data_response(_, Names) ->
    {ok, [{N, null, <<"id-a">>} || N <- Names]}.

valid_client_data_response(_, Names) ->
    {ok, [{N, <<"id-a">>, null} || N <- Names]}.

invalid_actor_data_response(_, Names) ->
    {ok, [{N, null, null} || N <- Names]}.

ambiguous_actor_data_response(_, Names) ->
    {ok, [{N, <<"id1">>, <<"id2">>} || N <- Names]}.


valid_authz_records_by_name(_Type, _OrgId, _Names) ->
     [ {<<"name1">>, <<"id1">>},
       {<<"name2">>, <<"id2">>} ].

valid_actors_only_ej() ->
    {
     [{<<"read">>,
       {[
         {<<"groups">>,[<<"x">>]},
         {<<"actors">>,[<<"a">>, <<"b">>, <<"c">>]}
        ]}
      }]
    }.

valid_actors_users_clients_ej() ->
    {
     [{<<"read">>,
       {[
         {<<"actors">>,[]},
         {<<"groups">>,[<<"x">>]},
         {<<"users">>,[<<"a">>,<<"b">>]},
         {<<"clients">>,[<<"c">>]}
        ]}
      }]
    }.
invalid_actors_users_clients_ej() ->
    {
     [{<<"read">>,
       {[
         {<<"actors">>,[<<"a">>,<<"b">>, <<"c">>]},
         {<<"groups">>,[<<"x">>]},
         {<<"users">>,[<<"a">>,<<"b">>]},
         {<<"clients">>,[<<"c">>]}
        ]}
      }]
    }.

invalid_clients_only_ej() ->
    {
     [{<<"read">>,
       {[
         {<<"actors">>,[]},
         {<<"groups">>,[<<"x">>]},
         {<<"clients">>,[<<"c">>]}
        ]}
      }]
    }.

invalid_users_only_ej() ->
    {
     [{<<"read">>,
       {[
         {<<"actors">>,[]},
         {<<"groups">>,[<<"x">>]},
         {<<"users">>,[<<"a">>,<<"b">>]}
        ]}
      }]
    }.
