%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @copyright 2011-2012 Opscode Inc.

-include_lib("webmachine/include/wm_reqdata.hrl").
-include_lib("chef_objects/include/chef_types.hrl").
-include_lib("chef_authz/include/chef_authz.hrl").
-include_lib("mixer/include/mixer.hrl").
-include_lib("ej/include/ej.hrl").

%% FIXME: this will be OPC only for the start most likely
-include_lib("stats_hero/include/stats_hero.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).
-endif.

-define(OSC_ORG_NAME, <<"open-source-chef">>).
-define(OSC_ORG_ID, <<"00000000000000000000000000000000">>).

%% Shared resource state shared by all chef_wm resource modules.
-record(base_state, {
          %% Concrete resource impl
          resource_mod :: atom(),
          %% unique request ID from nginx header (or generated if not
          %% found) set by chef_wm_util:read_req_id.
          reqid :: binary(),

          %% The name of the HTTP request header containing the unique ID set by the load
          %% balancer
          reqid_header_name :: string(),

          %% A fun/1 that closes over the request headers and returns
          %% header values as binaries or 'undefined'.
          %% chef_rest_wm:init sets this.
          header_fun = undefined :: fun((binary()|string()) -> binary() | 'undefined')
                                  | 'undefined',

          %% Message added to erchef log messages (not user visible).
          %% Used to pass extra info usually in non-200 cases to the
          %% shared request logging code.
          %%
          %% Formatted using ~p, but expected to be reasonably small.
          log_msg = "" :: term(),

          %% Set by chef_rest_wm:is_authorized to record whether
          %% request is originating from a user or a client.
          requester_type :: 'user' | 'client',

          %% Time drift in seconds allowed between the timestamp in a
          %% singed request and the clock on the server.  Set in
          %% app.config {chef_rest, auth_skey}.
          auth_skew = 900 :: non_neg_integer(),

          %% The GUID for the organization name that appears in the
          %% request URL.  This gets set in chef_rest_wm:is_authorized
          %% if a client is making the request.
          organization_guid :: object_id(),

          %% The name of the organization parsed from the request URL.
          %% Set by chef_rest_wm:service_available.
          organization_name :: binary(),

          %% Batch size used to pull back large objects from couchdb.
          %% Currently used by the search resource to limit the number
          %% of nodes that are in memory at one time.
          batch_size = 5 :: non_neg_integer(),

          %% Opaque db connection context record as returned by
          %% chef_db:make_context.  Allows db layer access to request
          %% ID.  Set in chef_rest_wm:service_available
          chef_db_context :: chef_db:db_context(),

          %% Opaque db connection context record as returned by chef_authz:make_context.
          chef_authz_context :: chef_authz:chef_authz_context(),

          %% Details for The actor making the request.
          requestor :: #chef_requestor{} | undefined,

          %% A record containing resource-specific state.
          resource_state :: tuple(),

          %% Type of database backend we are talking to
          db_type :: 'mysql' | 'pgsql',

          %% Turn this on if superuser is allowed to bypass security checks for
          %% this endpoint.
          superuser_bypasses_checks = false :: true | false
         }).

-record(search_state, {
          solr_query = undefined,
          partial_paths = []
         }).

-record(role_state, {
          %% EJson-encoded representation of a Role
          role_data,
          role_authz_id,
          chef_role :: #chef_role{}
         }).

-define(gv(X,L), proplists:get_value(X, L)).
-define(gv(X,L, D), proplists:get_value(X, L, D)).
