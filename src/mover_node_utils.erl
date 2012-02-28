-module(mover_node_utils).

-export([mark_node/2,
         mark_node/3,
         store_node/5]).

-include("mover.hrl").

mark_node(complete, Id) ->
    case dets:lookup(all_nodes, Id) of
        [] -> ok;
        [Node] ->
            Node1 = Node#node{status = mysql, solr = both},
            ok = dets:insert(all_nodes, Node1),
            Node1
    end;
mark_node(solr_clean, Id) ->
    case dets:lookup(all_nodes, Id) of
        [] -> ok;
        [#node{status = mysql, solr = both} = Node] ->
            Node1 = Node#node{solr = mysql},
            ok = dets:insert(all_nodes, Node1),
            Node1
    end.

mark_node(error, Id, Why) ->
    case dets:lookup(all_nodes, Id) of
        [] -> ok;
        [Node] ->
            Node1 = Node#node{status = {error, Why}},
            ok = dets:insert(all_nodes, Node1),
            Node1
    end.

store_node(Cn, OrgName, OrgId, NodeId, NodeName) ->
    Node = case chef_otto:fetch_by_name(Cn, OrgId, NodeName, authz_node) of
               {ok, MixlibNode} ->
                   MixlibId = ej:get({<<"_id">>}, MixlibNode),
                   %% Note that this can return a {not_found, _} tuple so we use
                   %% status_for_ids to validate that we have binaries and otherwise mark
                   %% node as an error.
                   AuthzId = chef_otto:fetch_auth_join_id(Cn, MixlibId, user_to_auth),
                   RequestorId = ej:get({<<"requester_id">>}, MixlibNode),
                   #node{id = NodeId,
                         name = NodeName,
                         org_id = OrgId,
                         authz_id = AuthzId,
                         requestor = RequestorId,
                         status = status_for_ids(AuthzId, RequestorId)};
               Error ->
                   #node{id = NodeId,
                         name = NodeName,
                         org_id = OrgId,
                         status = status_for_error(Error)}
           end,
    dets:insert(all_nodes, Node),
    log_node_stored(OrgName, Node),
    Node.

log_node_stored(OrgName, #node{status=couchdb, id=Id, name=Name, org_id=OrgId}) ->
    Self = pid_to_list(self()),
    fast_log:info(node_errors, Self, "node authz ok: ~s ~s ~s ~s",
                  [OrgName, Name, OrgId, Id]);
log_node_stored(OrgName, #node{status={error, {missing_authz, Type}},
                               id=Id, name=Name, org_id=OrgId}=Node) ->
    dets:insert(error_nodes, Node),
    Self = pid_to_list(self()),
    fast_log:err(node_errors, Self, "missing authz data (~p): ~s ~s ~s ~s",
                 [Type, OrgName, Name, OrgId, Id]);
log_node_stored(OrgName, #node{status={error, Why}, id=Id, name=Name, org_id=OrgId}=Node) ->
    dets:insert(error_nodes, Node),
    Self = pid_to_list(self()),
    fast_log:err(node_errors, Self, "node authz fail: ~s ~s ~s ~s ~p",
                 [OrgName, Name, OrgId, Id, Why]).

status_for_error({not_found, authz_node}) ->
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
