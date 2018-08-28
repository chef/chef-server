%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Tyler Cloke <tyler@chef.io>
%% @author Mark Anderson <mark@chef.io>
%% Copyright 2014-2018 Chef Software, Inc.

-module(oc_chef_organization).

-include("oc_chef_types.hrl").
-include_lib("mixer/include/mixer.hrl").
-include("chef_types.hrl").

-behaviour(chef_object).

-export([
         authz_id/1,
         is_indexed/1,
         ejson_for_indexing/2,
         update_from_ejson/2,
         set_created/2,
         set_updated/2,
         set_api_version/2,
         create_query/1,
         update_query/1,
         delete_query/1,
         find_query/1,
         list_query/1,
         bulk_get_query/1,
         fields_for_update/1,
         fields_for_fetch/1,
         record_fields/1,
         list/2,
         new_record/4,
         name/1,
         id/1,
         org_id/1,
         type_name/1,
         assemble_organization_ejson/1,
         parse_binary_json/1
        ]).

-mixin([{chef_object_default_callbacks, [ fetch/2, update/2 ]}]).

%% We don't have a class for 'organizations' on the client yet, but eventually we may want
%% to send a json_class Chef::ApiOrganization or the like.
%% Then we would amend DEFAULT_FIELD_VALUES and VALIDATION_CONSTRAINTS to include:
%% {<<"json_class">>, <<"Chef::Organization">>},
%% {<<"chef_type">>, <<"organization">>},
-define(DEFAULT_FIELD_VALUES, [ ]).
-define(GUID_FIELD, <<"guid">>).
-define(NAME_FIELD, <<"name">>).
-define(FULL_NAME_FIELD, <<"full_name">>).

-define(VALID_KEYS, [?NAME_FIELD, ?FULL_NAME_FIELD]).

validation_constraints(undefined) ->
    {[ {?NAME_FIELD, {string_match, regex_for(org_name)} },
       {?FULL_NAME_FIELD,  {string_match, regex_for(org_full_name)} }
     ]};
validation_constraints(OrgNameMatch) ->
    %% Note that this message appears to be overridden by a generic "field 'name' is invalid " response down the line.
    {[ {?NAME_FIELD, {string_match, {OrgNameMatch, <<"Invalid organization name. Organization name must match existing name.">>}} },
       {?FULL_NAME_FIELD,  {string_match, regex_for(org_full_name)} }
     ]}.

authz_id(#oc_chef_organization{authz_id = AuthzId}) ->
    AuthzId.

is_indexed(_ObjectRec) ->
    false.

ejson_for_indexing(#oc_chef_organization{}, _EjsonTerm) ->
   erlang:error(not_indexed).

update_from_ejson(#oc_chef_organization{} = Organization, OrganizationData) ->
    Name = ej:get({?NAME_FIELD}, OrganizationData),
    FullName = ej:get({?FULL_NAME_FIELD}, OrganizationData),
    Organization#oc_chef_organization{name = Name, full_name = FullName}.

set_created(#oc_chef_organization{} = Organization, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Organization#oc_chef_organization{created_at = Now, updated_at = Now, last_updated_by = ActorId}.

set_updated(#oc_chef_organization{} = Organization, ActorId) ->
    Now = chef_object_base:sql_date(now),
    Organization#oc_chef_organization{updated_at = Now, last_updated_by = ActorId}.

create_query(_ObjectRec) ->
    insert_organization.

update_query(_ObjectRec) ->
    update_organization_by_id.

delete_query(_ObjectRec) ->
    delete_organization_by_id.

find_query(_ObjectRec) ->
    find_organization_by_id.

list_query(_ObjectRec) ->
    list_organizations.

%% Not implemented because we have no serialized json body
bulk_get_query(_ObjectRec) ->
    erlang:error(not_implemented).

fields_for_update(#oc_chef_organization{last_updated_by = LastUpdatedBy,
                                        updated_at = UpdatedAt,
                                        name = Name,
                                        full_name = FullName,
                                        id = Id}) ->
    [LastUpdatedBy, UpdatedAt, Name, FullName, Id].

fields_for_fetch(#oc_chef_organization{id = Id}) ->
    [Id].

record_fields(_ApiVersion) ->
    record_info(fields, oc_chef_organization).

list(#oc_chef_organization{} = Org, CallbackFun) ->
    CallbackFun({list_query(Org), [], [name]}).

parse_binary_json({Bin, OrgName}) ->
    parse_binary_json(Bin, OrgName);
parse_binary_json(Bin) ->
    parse_binary_json(Bin, undefined).

parse_binary_json(Bin, OrgName) ->
    Org0 = chef_json:decode_body(Bin),
    Org = chef_object_base:set_default_values(Org0, ?DEFAULT_FIELD_VALUES),
    validate_org(Org, OrgName). %% TODO need action specific version?

validate_org(Org, OrgName) ->
    case ej:valid(validation_constraints(OrgName), Org) of
        ok -> {ok, Org};
        Bad -> throw(Bad)
    end.

%%
%% Open question: We don't expose json_class and chef_type fields currently, and the client doesn't have objects for them.
%% Is it worth sending at least chef_type fields?
%%
assemble_organization_ejson(#oc_chef_organization{id = Guid,
                                                  name = Name,
                                                  full_name = FullName }) ->
    Org = {[{?NAME_FIELD, Name},
            {?FULL_NAME_FIELD, FullName},
            {?GUID_FIELD, Guid} ]},
    chef_object_base:set_default_values(Org, ?DEFAULT_FIELD_VALUES).

new_record(ApiVersion, _OrgId, AuthzId, OrganizationData) ->
    Id = chef_object_base:make_guid(),

    Name = ej:get({?NAME_FIELD}, OrganizationData),
    FullName = ej:get({?FULL_NAME_FIELD}, OrganizationData),
    %% should default date be factored elsewhere? Other dates are set in chef_object_base.
    %% Also, does assigned at really make sense when org creation is done in one step?
    AssignedAt = ej:get({<<"assigned_at">>}, OrganizationData, chef_object_base:sql_date(now)),
    #oc_chef_organization{
       server_api_version = ApiVersion,
       id = Id,
       authz_id = AuthzId,
       name = Name,
       full_name = FullName,
       assigned_at = AssignedAt
      }.

name(#oc_chef_organization{name = Name}) ->
    Name.

id(#oc_chef_organization{id = Id}) ->
    Id.

org_id(#oc_chef_organization{}) ->
    erlang:error(not_implemented).

type_name(#oc_chef_organization{}) ->
    organization.

%%
%% TODO: This was copy-pasta'd from chef_regex, reunite someday (see also oc_chef_container)
%%
%% The name regex should limit to some short length. Nginx default is 8k. (large_client_header_buffers)
%% but we probably for sanity's sake want something less. 255 seems reasonable, but we probably should dig deeper.
%% For reference, I've succesfully created orgs of 900+ character lengths in hosted, but failed with 1000.
%% Probably have to be able to handle api.opscode.us/<ORGNAME>/environments/default in 1k?)
%%
%% We use [a-z] instead of [:lower:] because the latter can change meaning if we set PCRE_UCP when compiling the regex.
%%
%% The org name is stricter than the lb allows (right now; it doesn't block upper/digit/-_ for first character)
%% This strictness matches the previous ruby implementation, except that it didn't limit the length.
-define(ORG_NAME_REGEX, "[a-z0-9][a-z0-9_-]{0,254}").
%%
%% Full name is free text, except that it must start with nonspace.
%%
-define(FULL_NAME_REGEX, "\\S.{0,1022}").
-define(ANCHOR_REGEX(Regex), "^" ++ Regex ++ "$").

generate_regex(Pattern) ->
  {ok, Regex} = re:compile(Pattern),
  Regex.

generate_regex_msg_tuple(Pattern, Message) ->
  Regex = generate_regex(Pattern),
  {Regex, Message}.

regex_for(org_name) ->
    generate_regex_msg_tuple(?ANCHOR_REGEX(?ORG_NAME_REGEX),
                             <<"Malformed org name.  Must only contain A-Z, a-z, 0-9, _, or -">>);
regex_for(org_full_name) ->
    generate_regex_msg_tuple(?ANCHOR_REGEX(?FULL_NAME_REGEX),
                             <<"Malformed org full name.  Must only contain A-Z, a-z, 0-9, _, or -">>).

set_api_version(ObjectRec, Version) ->
    ObjectRec#oc_chef_organization{server_api_version = Version}.
