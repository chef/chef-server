-module(chef_index_test_utils).

-compile(export_all).

read_file(File) ->
    file:read_file(filename(File)).

filename(File) ->
    filename:join([".", "apps", "chef_index", "test", File]).
