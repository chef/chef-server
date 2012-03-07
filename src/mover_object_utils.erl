-module(mover_object_utils).

-export([mark_object/3,
         mark_object/4,
         fetch_object_authz/5,
         table_name/1,
         error_table_name/1]).

-include("mover.hrl").

mark_object(Table, complete, Id) ->
    case dets:lookup(Table, Id) of
        [] -> ok;
        [Object] ->
            Object1 = Object#object{status = mysql, solr = both},
            ok = dets:insert(Table, Object1),
            Object1
    end;
mark_object(Table, solr_clean, Id) ->
    case dets:lookup(Table, Id) of
        [] -> ok;
        [#object{status = mysql, solr = both} = Object] ->
            Object1 = Object#object{solr = mysql},
            ok = dets:insert(Table, Object1),
            Object1
    end.

mark_object(Table, error, Id, Why) ->
    case dets:lookup(Table, Id) of
        [] -> ok;
        [Object] ->
            Object1 = Object#object{status = {error, Why}},
            ok = dets:insert(Table, Object1),
            Object1
    end.

table_name(Name) ->
    list_to_atom("all_" ++ atom_to_list(Name) ++ "s").

error_table_name(Name) ->
        list_to_atom("error_" ++ atom_to_list(Name) ++ "s").


fetch_object_authz(Cn, OrgId, ObjectId, ObjectName, AuthzType) ->
    case chef_otto:fetch_by_name(Cn, OrgId, ObjectName, AuthzType) of
        {ok, MixlibObject} ->
            MixlibId = ej:get({<<"_id">>}, MixlibObject),
            %% Note that this can return a {not_found, _} tuple so we use
            %% status_for_ids to validate that we have binaries and otherwise mark
            %% object as an error.
            AuthzId = chef_otto:fetch_auth_join_id(Cn, MixlibId, user_to_auth),
            RequestorId = ej:get({<<"requester_id">>}, MixlibObject),
            #object{id = ObjectId,
                    name = ObjectName,
                    org_id = OrgId,
                    authz_id = AuthzId,
                    requestor = RequestorId,
                    status = status_for_ids(AuthzId, RequestorId)};
        Error ->
            #object{id = ObjectId,
                    name = ObjectName,
                    org_id = OrgId,
                    status = status_for_error(Error)}
    end.

status_for_error({not_found, authz_object}) ->
    {error, {missing_authz, no_mixlib_doc}};
status_for_error(Error) ->
    {error, Error}.

status_for_ids(AuthzId, RequestorId) when is_binary(AuthzId),
                                          is_binary(RequestorId) ->
    {ok, Regex} = re:compile("[a-f0-9]{32}"),
    case {re:run(AuthzId, Regex), re:run(RequestorId, Regex)} of
        {{match, _}, {match, _}} -> couchdb;
        _NoMatch -> 
            {error,
             {bad_id_format, [{authz_id, AuthzId}, {requestor, RequestorId}]}}
    end;
status_for_ids({not_found, missing}, _RequestorId) ->
    {error, {missing_authz, no_auth_join}};
status_for_ids(AuthzId, RequestorId) ->
    {error, {unknown, [{authz_id, AuthzId}, {requestor, RequestorId}]}}.
