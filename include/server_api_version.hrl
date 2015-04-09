
%% Define supported server versions here. This will allow their usage
%% in pattern matches, etc to be clear in meaning.

% Baseline of all behaviors as of 12.0.6
-define(API_v0, 0).

% Adds new client/user/keys endpoint behaviors.  Removes certificate support.
% Changes reported X-Ops-API-Version header info to match with the actual API version
% and not product version.
-define(API_v1, 1).

%% Which level of API version is deprecated? Not currently used by
%% erchef, this is for human consumption.
-define(API_DEPRECATED_VER, ?API_v0).


-define(API_MIN_VER, ?API_v0).
-define(API_MAX_VER, ?API_v1).

-type api_version() :: non_neg_integer() | -1.

