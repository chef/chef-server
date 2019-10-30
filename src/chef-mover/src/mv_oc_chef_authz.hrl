%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VENDORED Records and Types
%%
%% Replaces dependency in oc_chef_authz:
%%      -include("oc_chef_authz.hrl").
%%      -include("oc_chef_authz_db.hrl").
%%
%% Copied from oc_erchef 1.6.4 d0fa517e23483ca9555721a0ebae681e30f9d104
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-type resource_type() :: 'actor'|'container'|'group'|'object'.
-type access_method() :: 'create'|'read'|'update'|'delete'|'grant'.
-type actor_list() :: [ binary() ].
-type group_list() :: [ binary() ].

-type oc_authz_id() :: <<_:256>>.

-record(authz_group, {actors = [] :: actor_list(),
		              groups = [] :: group_list()}).

-record(authz_ace,  {actors = [] :: actor_list(),
		             groups = [] :: group_list()}).

-type authz_ace() :: #authz_ace{}.
-type authz_acl() :: [{access_method(), #authz_ace{}},...].

-define(access_methods, [create, read, update, delete, grant]).

-record(chef_container, {
          'id',             % guid for object (unique)
          'authz_id',       % authorization guid (unique)
          'org_id',         % organization guid
          'name',           % name of container
          'path',           % 'path' of container (not used? Orig part of inheritance mech?; safe to delete? Yea!)
          'last_updated_by' % authz guid of last actor to update object
         }).

-record(oc_chef_authz_context,
        {reqid :: binary() | undefined,
         otto_connection :: couchbeam:server() | undefined,
         darklaunch :: term() | undefined}).

-type oc_chef_authz_context() :: #oc_chef_authz_context{}.

-type requestor_id() :: binary().
-type actor_id() :: binary().
-type object_id() :: <<_:256>>.
-type db_key() :: binary() | string().

-type authz_type() :: 'authz_client' |
                      'authz_container' |
                      'authz_cookbook' |
                      'authz_data_bag' |
                      'authz_environment' |
                      'authz_group' |
                      'authz_node' |
                      'authz_role'.

-type container_name() :: binary().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% END VENDORED Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

