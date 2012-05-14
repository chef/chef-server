%%%-------------------------------------------------------------------
%%% @author Eric B Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012, Opscode, Inc.
%%% @doc
%%% Opset provides a 'Configuration' service to other applications
%%% within an erlang node (right now its limited to a single node).
%%% This is a pretty simple key value store service with the exception
%%% that it notifies any linked process when the configuration
%%% changes. So the linked pids have the opportunity to reconfigure
%%% themselves.
%%%
%%% The reconfiguration messages are composed of two elements a
%%% 'delete' element and a 'set' element. So each notification message
%%% will consist of a list of one or more of these elements. The spec
%%% for a change indicater message can be seen in the @type
%%% change_indicator.
%%% @end
%%% -------------------------------------------------------------------
-module(opset).

-export([create/2,
         delete/1,
         exists/1,
         link_config/1,
         unlink_config/1,
         delete_value/2,
         get_value/2,
         get_value/3,
         set_value/3,
         set_values/2,
         reset_config/2]).

%% This is exported for internal use in opset, it should not be used
%% outside of this application.
-export([config_pid/1, form_key/1, event_name/1]).

-export_type([name/0, config/0, change_indicator/0, change_element/0]).

%%====================================================================
%% Types
%%====================================================================

-type name() :: atom().
-type config() :: proplist:proplists().
-type change_element() :: {delete, config()} | {set, config()}.
-type change_indicator() :: [change_element()].

%%====================================================================
%% External API
%%====================================================================

%% @doc create a new configuration
-spec create(name(), config()) -> ok.
create(Name, Value) ->
    ops_sup:start_config(Name, Value).

%% @doc delete a configuration
-spec delete(name()) -> ok.
delete(Name) ->
    gen_server:cast(config_pid(Name), delete).

%% @doc check to see that a named configuration exists
-spec exists(name()) -> boolean().
exists(Name) ->
   config_pid(Name) =/= undefined.

%% @doc link to the named configuration so that your process gets
%% messages when the configuration changes
-spec link_config(name()) -> ok.
link_config(Name) ->
    %% We need to link the calling process to a property name in gproc
    %% (you can have n pids related to 1 property). To do that
    %% gproc:reg must be called in the context of the process that
    %% that will link to the name
    gproc:reg({p, l, event_name(Name)}).

%% @doc unlink a previously linked process from the configuration.
-spec unlink_config(name()) -> ok.
unlink_config(Name) ->
    gproc:unreg({p, l, {?MODULE, Name}}).

%% @doc delete a value specified by 'ItemName' from the
%% configuration. A delete notification will be propagated.
-spec delete_value(term(), name()) -> term().
delete_value(ItemName, Name) ->
    gen_server:cast(config_pid(Name), {delete, ItemName}).

%% @doc get a value identified by 'ItemName' from the config. This
%% will return the value or if the value otherwise 'not_found
-spec get_value(term(), name()) -> term().
get_value(ItemName, Name) ->
   gen_server:call(config_pid(Name), {get, ItemName}).

%% @doc get_value Get the value specified by ItemName, if the value
%% does not exist then return the term specified by DefaultValue.
-spec get_value(term(), term(), name()) -> term().
get_value(ItemName, DefaultValue, Name) ->
    gen_server:call(config_pid(Name), {get, ItemName, DefaultValue}).

%% @doc set_value. Set the value ItemValue with key ItemName, a set
%% notification is propagated. This propagates a notification each
%% time its called. If you are setting mulitple values at the same
%% time its much better to use set_values/2.
-spec set_value(term(), term(), name()) -> term().
set_value(ItemName, ItemValue, Name) ->
    gen_server:cast(config_pid(Name), {set, ItemName, ItemValue}).

%% @doc sets multiple values at the same time. This is the preferred
%% way to change several values all at once. A single notification is
%% propogated for all values.
-spec set_values(config(), name()) -> ok.
set_values(Items, Name) ->
    gen_server:cast(config_pid(Name), {set, Items}).

%% @doc Setting this completely deletes all values in the config and
%% repopulates the config with the values set. A single notification
%% is sent with a 'delete' entry for those items deleted and a 'set'
%% entry for those items that are new or set.
-spec reset_config(config(), name()) -> ok.
reset_config(Config, Name) ->
    gen_server:cast(config_pid(Name), {reset, Config}).

%%====================================================================
%% Internal API
%%====================================================================

%% @doc This exists for internal use and should not be called by
%% external apps. It returns the pid for the named config.
-spec config_pid(name()) -> pid().
config_pid(Name) ->
    gproc:whereis_name(form_key(Name)).

%% @doc Convert the name to a value that gproc will understand
-spec form_key(name()) -> gproc:key().
form_key(Name) ->
    {n, l, Name}.
%% @doc Convert the name into a useful event name for gproc.
-spec event_name(name()) -> {module, name()}.
event_name(Name) ->
    {?MODULE, Name}.
