
%% Define supported server versions here. This will allow their usage
%% in pattern matches, etc to be clear in meaning.

% Baseline of all behaviors as of 12.0.6
-define(API_v0, 0).

% Adds new client/user/keys endpoint behaviors.  Removes certificate support.
% Changes reported X-Ops-API-Version header info to match with the actual API version
% and not product version.
-define(API_v1, 1).

% Deprecates cookbook segments
-define(API_v2, 2).

%% Highest level of deprecated API version that will be removed in the nxt major product release.
%% This is used in eunit tests for version range testing across objects.
-define(API_DEPRECATED_VER, ?API_v0).


-define(API_MIN_VER, ?API_v0).
-define(API_MAX_VER, ?API_v2).

-type api_version() :: non_neg_integer() | -1.
