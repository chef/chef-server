%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Seth Falcon <seth@opscode.com>
%% @author Christopher Maier <cm@opscode.com>
%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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

-module(chef_data_bag).

-export([
         id/1,
         name/1,
         type_name/1,
         new_record/3,
         parse_binary_json/2
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-include("chef_types.hrl").

%% @doc Describes the valid structure of a data bag for use with `ej:valid/2`.
-define(VALIDATION_CONSTRAINTS,
        {[
          {<<"name">>, {string_match, chef_regex:regex_for(data_bag_name)}}
          %% In other objects we would have validation like this:
          %%
          %%     {{opt, <<"chef_type">>}, <<"data_bag">>},
          %%     {{opt, <<"json_class">>}, <<"Chef::DataBag">>}
          %%
          %% We don't need that for data bags, because all we save from them is a name.  They
          %% have no 'serialized object' that is saved and also returned to clients upon
          %% retrieval.
         ]}).

-behaviour(chef_object).

-spec name(#chef_data_bag{}) -> binary().
name(#chef_data_bag{name = Name}) ->
    Name.

-spec id(#chef_data_bag{}) -> object_id().
id(#chef_data_bag{id = Id}) ->
    Id.

%% TODO: this doesn't need an argument
type_name(#chef_data_bag{}) ->
    data_bag.

-spec new_record(object_id(), object_id(), binary() | string()) -> #chef_data_bag{}.
new_record(OrgId, AuthzId, Name) ->
    Id = chef_object_base:make_org_prefix_id(OrgId, Name),
    #chef_data_bag{id = Id,
                   authz_id = chef_object_base:maybe_stub_authz_id(AuthzId, Id),
                   org_id = OrgId,
                   name = Name}.

%% @doc Convert a binary JSON string representing a Chef data_bag into an EJson-encoded
%% Erlang data structure.
%%
%% The `create` atom is required to keep the contract of this function in line with those of
%% other "Chef object" modules.  They allow for updates as well as creation, but updates
%% aren't valid for data bags.
-spec parse_binary_json(Bin :: binary(), Action :: create ) ->
                               {ok, Parsed :: ejson_term()}. % or throw
parse_binary_json(Bin, _Action=create) ->
    DataBag = chef_json:decode(Bin),
    validate_data_bag(DataBag).

validate_data_bag(DataBag) ->
    case ej:valid(?VALIDATION_CONSTRAINTS, DataBag) of
        ok ->
            {ok, DataBag};
        Bad ->
            throw(Bad)
    end.
