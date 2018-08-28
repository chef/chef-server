%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% Copyright 2012-2018 Chef Software, Inc.
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

%% @doc Utility functions to support the construction of stats_hero
%% metric labels.
-module(chef_metrics).

-export([
         label/2
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

%% @doc Generate a label for stats_hero based on an `Upstream' name
%% and a `{Module, Function}' pair.
%%
%% `Upstream' refers to the broad domain of external-service
%% functionality the metric corresponds to (e.g., `rdbms', `solr',
%% etc.), while `{Mod, Fun}' refers to the specific module /
%% function under measurement that implements some aspect of that
%% functionality.
-spec label(Upstream :: atom(),
            {Mod :: atom(), Fun :: atom()}) -> Label :: <<_:16,_:_*8>>.
label(s3=Upstream, {Mod, Fun}) when is_atom(Mod),
                                    is_atom(Fun) ->
    %% S3-related labels are special snowflakes because we want to
    %% incorporate not only the upstream, module, and function, but
    %% also the storage server host and the bucket name.
    %%
    %% The host will show whether we're hitting Bookshelf or actual
    %% S3, and the bucket will allow us to ascertain the impact of
    %% using different S3 regions on latency and performance.
    %%
    %% Because data.
    Url = envy:get(chef_objects, s3_url, string),
    Bucket = envy:get(chef_objects, s3_platform_bucket_name, string),

    %% These two components need to have '.' characters stripped so
    %% that we don't inadvertently introduce new hierarchy levels into
    %% our Graphite metric labels
    BucketBin = sanitize_label_component(Bucket),
    HostBin = sanitize_label_component(extract_host(Url)),

    %% TODO: In the future, we will likely be refactoring label
    %% generation such that the host and bucket information will be
    %% looked up once in the supervisor, and passed through in a "blob
    %% of static data" down to this function.  This means that we will
    %% no longer need to use application:get_env/2 here.  It also
    %% means that the host-extraction and component sanitization will
    %% take place in the supervisor, as well.
    %%
    %% Just an FYI.

    {UpstreamBin, ModBin, FunBin} = to_binaries(Upstream, Mod, Fun),

    %% e.g. <<s3.chef_mycompany_com.my_bucket.chef_s3.delete_checksums>>
    <<UpstreamBin/binary, ".",
      HostBin/binary, ".",
      BucketBin/binary, ".",
      ModBin/binary, ".",
      FunBin/binary>>;
label(Upstream, {Mod, Fun}) when is_atom(Upstream),
                                 is_atom(Mod),
                                 is_atom(Fun) ->
    %% All the other upstream-related metric labels are just simple
    %% concatenations of Upstream, Mod, and Fun.
    {UpstreamBin, ModBin, FunBin} = to_binaries(Upstream, Mod, Fun),
    <<UpstreamBin/binary, ".", ModBin/binary, ".", FunBin/binary>>.

-spec extract_host(Url :: list()) -> Host :: list().
extract_host(Url) when is_list(Url) ->
    %% Regex matches, e.g.:
    %%
    %% http://foo.bar.com
    %% https://foo.bar.com
    %% http://foo.bar.com:1234
    %% https://foo.bar.com:1234
    %% http://foo.bar.com/x/y/z
    %% https://foo.bar.com:1234/x/y/z
    %%
    %% etc.,and captures just the hostname portion.
    %%
    %% In actual use, there probably won't ever be a path component,
    %% or even a port number, but it pays to be paranoid.
    %%
    %% (NB: This is an 'extended' regex)
    Pattern = "(?:http|https)://  # protocol
               ([^:/]+)           # hostname (the only capturing group in the regex)
               (?::\d+)?          # optional port (i.e., colon followed by digits)
               (?:/.*)?           # optional path component (everything after a slash)",

    %% The 'extended' directive applies to the regex pattern
    %% compilation, and is required so the pattern above doesn't blow
    %% up :)
    %%
    %% The 'all_but_first' capture directive returns only explicitly
    %% captured sub-patterns (i.e., throws out the first match, which
    %% would be the whole portion of the input that matches the
    %% pattern).  Since we only have one capturing group, this will
    %% give us a single-element list of captures.
    %%
    %% The 'list' capture directive ensures that the captured group is
    %% returned as a string, as opposed to an index or a binary.
    {match, [Host]} = re:run(Url, Pattern,
                             [extended,
                              {capture, all_but_first, list}]),
    Host.

%% @doc Makes the given string a valid single path component of a
%% Graphite metrics label.
%%
%% Basically, ensures there no '.' characters (which would turn a
%% single component into several).  Currently used for sanitizing the
%% hostname and bucket components of the S3-related metrics.
%%
%% E.g., "www.foo.com" becomes "www_foo_com"
%%
%% Returns a binary for ease of inclusion in a (binary) metrics label.
-spec sanitize_label_component(Component :: list()) -> Sanitized :: binary().
sanitize_label_component(Component) when is_list(Component) ->
    re:replace(Component,
               "\\.",                       %% Replace literal '.' characters
               "_",                         %% with '_' characters
               [global, {return, binary}]). %% everywhere, and return as a binary

%% @doc Utility function to convert a bunch of atoms to binaries for
%% inclusion in metrics labels.
to_binaries(Upstream, Mod, Fun) when is_atom(Upstream),
                                     is_atom(Mod),
                                     is_atom(Fun) ->

    %% Since these are destined to be individual metrics label
    %% components, in order to technically and paranoically correct,
    %% we could / should also sanitize them as well, but you'd have to
    %% be kind of twisted and self-hating to configure your system in
    %% such a way as to make that necessary.
    UpstreamBin = erlang:atom_to_binary(Upstream, utf8),
    ModBin = erlang:atom_to_binary(Mod, utf8),
    FunBin = erlang:atom_to_binary(Fun, utf8),

    {UpstreamBin, ModBin, FunBin}.
