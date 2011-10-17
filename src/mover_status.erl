-module(mover_status).

-export([org_by_name/1]).

-include("mover.hrl").

org_by_name(Name) ->
     Spec = #org{guid = '_',
                 name = Name,
                 preloaded = '_',
                 read_only = '_',
                 active = '_',
                 migrated = '_',
                 worker = '_'},
    dets:match_object(all_orgs, Spec).
    

