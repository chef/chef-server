%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Christopher Brown
%% @author Christopher Maier <cm@chef.io>
%% @author Seth Falcon <seth@chef.io>
%% @author Ho-Sheng Hsiao
%% Copyright 2011-2015 Chef Software, Inc. All Rights Reserved.
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

-module(chef_wm_util).

-export([base_uri/1,
         environment_not_found_message/1,
         error_message_envelope/1,
         extract_from_path/2,
         get_header_fun/2,
         fetch_org_metadata/1,
         base_mods/0,
         not_found_message/2,
         num_versions/1,
         num_versions/2,
         object_name/2,
         set_json_body/2,
         set_uri_of_created_resource/2,
         set_location_of_created_resource/2,
         with_error_body/2
        ]).

-include("oc_chef_wm.hrl").

%% TODO: These types are just placeholders until we get all the types cleaned up
-type ejson() :: {[tuple()]}.

%% @doc Report the names of the "pluggable" modules determined at compile time. Useful for
%% debugging builds.
base_mods() ->
    [{base_resource, oc_chef_wm}, {base_routes, oc_chef_wm_routes}].

%% @doc Returns the base URI for the server as called by the client as a string.
%% By default, use the Host: header value, or if configured with the atom 'host_header'
%% Otherwise, use the configured url.
-spec base_uri(wm_req()) -> string().
base_uri(Req) ->
    case application:get_env(oc_chef_wm, base_resource_url) of
        undefined         -> scheme(Req) ++ "://" ++ vhost(Req);
        {ok, host_header} -> scheme(Req) ++ "://" ++ vhost(Req);
        {ok, ExternalUrl} -> ExternalUrl
    end.

%% @doc Returns the host from the request headers, or use the webmachine default
vhost(Req) ->
    case wrq:get_req_header("host", Req) of
        B when is_binary(B) -> binary_to_list(B);
        S when is_list(S) -> S;
        undefined -> fqdn_with_port(Req)
    end.

%% @doc Returns the host via webmachine
fqdn_with_port(Req) ->
    string:join(wrq:host_tokens(Req), ".") ++ port_string(wrq:port(Req)).

get_header_fun(Req, State = #base_state{header_fun = undefined}) ->
    GetHeader = fun(H) ->
                        Name = case is_binary(H) of
                                   true -> binary_to_list(H);
                                   false -> H
                               end,
                        case wrq:get_req_header(string:to_lower(Name), Req) of
                            B when is_binary(B) -> B;
                            "" -> undefined; %% We want to treat empty header values as missing
                            % Possible DOS attack if this header includes a massive string and possibility
                            % of exhausting the heap?
                            S when is_list(S) -> iolist_to_binary(S);
                            undefined -> undefined
                        end
                end,
    {GetHeader, State#base_state{header_fun = GetHeader}};
get_header_fun(_Req, State) ->
    {State#base_state.header_fun, State}.

fetch_org_metadata(#base_state{organization_name = undefined}) ->
    {undefined, undefined};
fetch_org_metadata(#base_state{organization_guid = Id, organization_authz_id=AuthzId}) when is_binary(Id) ->
    {Id, AuthzId};
fetch_org_metadata(#base_state{organization_guid = undefined,
                               organization_name = OrgName,
                               chef_db_context = DbContext}) ->
    case chef_db:fetch_org_metadata(DbContext, OrgName) of
        not_found -> throw({org_not_found, OrgName});
        {Guid, AuthzId} -> {Guid, AuthzId}
    end.

-spec environment_not_found_message( bin_or_string() ) -> ejson().
environment_not_found_message(EnvName) ->
    error_message_envelope(iolist_to_binary([<<"Cannot load environment '">>, EnvName, <<"'">>])).

%% TODO: Why are these messages phrased differently?  This is what the Ruby endpoints were
%% doing, FYI.  Is this something we're stuck with, or can we update the API and normalize
%% these messages?
-spec not_found_message( node | role | group | data_bag | data_bag_item1 |
                         data_bag_item2 | client | data_bag_missing_for_item_post |
                         environment | sandbox | sandboxes | cookbook |
                         cookbook_version | user | invitation | policy | policy_group,
                         bin_or_string() | {bin_or_string(), bin_or_string()} ) -> ejson().
not_found_message(node, Name) ->
    error_message_envelope(iolist_to_binary(["node '", Name, "' not found"]));
not_found_message(role, Name) ->
    error_message_envelope(iolist_to_binary(["Cannot load role ", Name]));
not_found_message(group, Name) ->
    error_message_envelope(iolist_to_binary(["group '", Name, "' not found"]));
not_found_message(data_bag, Name) ->
    error_message_envelope(iolist_to_binary(["Cannot load data bag ", Name]));
not_found_message(data_bag_item1, {BagName, ItemName}) ->
    error_message_envelope(iolist_to_binary(["Cannot load data bag ", BagName, " item ",
                                             ItemName]));
not_found_message(data_bag_item2, {BagName, ItemName}) ->
    error_message_envelope(iolist_to_binary(["Cannot load data bag item ", ItemName,
                                             " for data bag ", BagName]));
not_found_message(data_bag_missing_for_item_post, BagName) ->
    error_message_envelope(iolist_to_binary(["No data bag '",
                                             BagName,
                                             "' could be found. Please create this ",
                                             "data bag before adding items to it."]));
not_found_message(sandbox, SandboxId) ->
    error_message_envelope(iolist_to_binary([<<"No such sandbox '">>, SandboxId,
                                             <<"'.">>]));
not_found_message(environment, Name) ->
    error_message_envelope(iolist_to_binary(["Cannot load environment ", Name]));
not_found_message(organization, Name) ->
    error_message_envelope(iolist_to_binary(["Cannot load organization ", Name]));
not_found_message(cookbook, Name) when is_binary(Name) ->
    error_message_envelope(iolist_to_binary(["Cannot find a cookbook named ", Name]));
not_found_message(cookbook_version, {Name, Version}) when is_binary(Version) -> %% NOT a parsed {Major, Minor, Patch} tuple!!
    error_message_envelope(iolist_to_binary(["Cannot find a cookbook named ", Name, " with version ", Version]));
not_found_message(client, Name) ->
    error_message_envelope(iolist_to_binary(["Cannot load client ", Name]));
not_found_message(user, Name) ->
    {[{<<"error">>, iolist_to_binary(["Could not find user ", Name])}]};
not_found_message(association, {Name, OrgName} ) ->
    {[{<<"error">>, iolist_to_binary(["Cannot find a user ", Name, " in organization ", OrgName])}]};
not_found_message(invitation, Id) ->
    {[{<<"error">>, iolist_to_binary(["Cannot find association request: ", Id])}]};
not_found_message(key, {OwnerName, KeyName}) ->
    {[{<<"error">>, iolist_to_binary(["There is no key named ", KeyName, " associated with ", OwnerName, "."])}]};
not_found_message(policy, Name) ->
    error_message_envelope(iolist_to_binary(["policy '", Name, "' not found"]));
not_found_message(policy_group, Name) ->
    error_message_envelope(iolist_to_binary(["policy_group '", Name, "' not found"])).



%% "Cannot load data bag item not_really_there for data bag sack"

-spec error_message_envelope(binary() | list() | ejson_term()) -> ejson_term().
error_message_envelope(Message) when is_list(Message) ->
    error_message_envelope(iolist_to_binary(Message));
error_message_envelope(Message) when is_binary(Message) orelse
                                     is_tuple(Message) ->
    %% Tuple guard is really intended for grabbing EJson-encoded json objects, but we don't
    %% have guards for that.  It was added to accommodate depsolver messages.  This is part
    %% of an ongoing refactor, and may not ultimately be necessary.
    {[{<<"error">>, [Message]}]}.

%% @doc Converts the given Ejson-encoded data to a JSON string and
%% sets it as the request body, returning the updated request.
%% @end
set_json_body(Req, EjsonData) ->
    Json = chef_json:encode(EjsonData),
    wrq:set_resp_body(Json, Req).

%% @doc Convenience method for manipulating error data.  Data that is passed in is wrapped
%% in an error message envelope, which is then encoded to JSON and set as the body of the
%% request.  This updated request is returned.
-spec with_error_body(Req :: wm_req(),
                      ErrorData :: jiffy:json_value()) ->
                             ReqWithErrorJSON :: wm_req().
with_error_body(Req, ErrorData) ->
    ErrorPayload = error_message_envelope(ErrorData),
    set_json_body(Req, ErrorPayload).

%% @doc Sets the JSON body of a response and it's Location header to
%% point to the URI of a newly-created resource.
%%
%% The body will be of the form
%% ```
%%     {"uri":"http://foo.com/newresource"}
%% '''
%% Returns the updated request.
set_uri_of_created_resource(Uri, Req) when is_list(Uri) ->
    set_uri_of_created_resource(list_to_binary(Uri), Req);
set_uri_of_created_resource(Uri, Req0) when is_binary(Uri) ->
    %% Uri needs to be a binary for encoding to JSON, but a string for the header value
    Req = set_json_body(Req0, {[{<<"uri">>, Uri}]}),
    set_location_of_created_resource(Uri, Req).

set_location_of_created_resource(Uri, Req0) when is_list(Uri) ->
    set_location_of_created_resource(list_to_binary(Uri), Req0);
set_location_of_created_resource(Uri, Req0) when is_binary(Uri) ->
    wrq:set_resp_header("Location", binary_to_list(Uri), Req0).

%% @doc Extracts the name of a given object from the request path.  This is for use in
%% resources that manipulate individual Chef objects, like nodes or roles.
%%
%% For example, given a request to the path `"/nodes/foo"', this
%% function would return `<<"foo">>'
%% @end
%%
%% TODO: Currently we only use this for nodes and roles; when we clean up our custom types,
%% the spec will be updated
-spec object_name(cookbook | node | role | data_bag | data_bag_item |
                  environment | principal | sandbox | client | user |
                  group | container | organization | invitation | key |
                  policy | policy_group,
                  Request :: #wm_reqdata{}) -> binary() | undefined.
object_name(node, Req) ->
    extract_from_path(node_name, Req);
object_name(role, Req) ->
    extract_from_path(role_name, Req);
object_name(data_bag, Req) ->
    extract_from_path(data_bag_name, Req);
object_name(data_bag_item, Req) ->
    extract_from_path(item_name, Req);
object_name(sandbox, Req) ->
    extract_from_path(sandbox_id, Req);
object_name(environment, Req) ->
    extract_from_path(environment_name, Req);
object_name(principal, Req) ->
    extract_from_path(principal_name, Req);
object_name(cookbook, Req) ->
    extract_from_path(cookbook_name, Req);
object_name(group, Req) ->
    extract_from_path(group_name, Req);
object_name(container, Req) ->
    extract_from_path(container_name, Req);
object_name(client, Req) ->
    extract_from_path(client_name, Req);
object_name(user, Req) ->
    extract_from_path(user_name, Req);
object_name(organization, Req) ->
    extract_from_path(organization_id, Req);
object_name(invitation, Req) ->
    extract_from_path(invitation_id, Req);
object_name(key, Req) ->
    extract_from_path(key_name, Req);
object_name(policy, Req) ->
    extract_from_path(policy_name, Req);
object_name(policy_group, Req) ->
    extract_from_path(policy_group_name, Req);
object_name(cookbook_artifact, Req) ->
    extract_from_path(cookbook_artifact_name, Req);
%% For requests to `/policy_groups/:policy_group/policies/:policy_name`,
%% the lookup key is `policy_group` without the `_name`
object_name(policy_group_asoc_name, Req) ->
    extract_from_path(policy_group, Req).



%% @doc Private utility function to extract a path element as a binary.  Returns the atom
%% `undefined' if no such value exists.
extract_from_path(PathKey, Req) ->
    case wrq:path_info(PathKey, Req) of
        undefined ->
            undefined;
        Value ->
            list_to_binary(Value)
    end.

%% @doc Utility function to process the `num_versions' parameter that is common to several
%% cookbook-related resources
-spec num_versions(Req :: #wm_reqdata{}) ->  all | non_neg_integer().
num_versions(Req) ->
    %% Assume a default value of 1 if the Request has no `num_versions' query parameter
    num_versions(1, Req).

%% @doc Depending on which resource the current request is processing, the default value for
%% an unspecified 'num_versions' parameter can be either 1 or `all'.
-spec num_versions(Default :: 1 | all,
                   Req :: wm_req()) ->  all | non_neg_integer().
num_versions(Default, Req) ->
    case wrq:get_qs_value("num_versions", Req) of
        undefined ->
            Default;
        "all" ->
            all;
        Str ->
            parse_number(Str)
    end.

%% @doc Validate a num_versions parameter.  We only allow
%% non_negative numbers of versions to be displayed
-spec parse_number(string()) -> non_neg_integer().
parse_number(Str) when is_list(Str) ->
    case string:to_integer(Str) of
        {error, _} ->
            throw(invalid_num_versions);
        {Num, []} when Num >= 0 ->
            Num;
        _ ->
            throw(invalid_num_versions)
    end.

scheme(Req) ->
    case wrq:get_req_header("x-forwarded-proto", Req) of
        undefined ->
            erlang:atom_to_list(wrq:scheme(Req));
        Proto -> Proto
    end.

%% So this is kind of gross and will prevent correct port info if you run https on port 80
%% or http on port 443; otherwise it should work. The problem is two-fold, first webmachine
%% ignores scheme information when parsing the host header and so always sets the port to 80
%% if no port is present in the host header. But in a load-balanced situation, the scheme
%% from webmachine may not reflect what is in use at the load balancer. A simple compromise
%% is to treat both 80 and 443 as default and only include a port string if the port differs
%% from those.
port_string(Default) when Default =:= 80; Default =:= 443 ->
    "";
port_string(Port) ->
    [$: | erlang:integer_to_list(Port)].

