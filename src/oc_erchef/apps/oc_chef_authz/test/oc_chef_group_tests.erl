%% @author Ho-Sheng Hsiao <hosh@chef.io>
%% @doc group operations
%%
%% Copyright 2013 Opscode, Inc. All Rights Reserved.
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

-module(oc_chef_group_tests).

-compile([export_all]).

-include_lib("eunit/include/eunit.hrl").

handle_error_test_() ->
    [
        { "When all requests are successful",
            fun() ->
                    OpsResults = [
                        {ok, chef_test_suite_helper:make_az_id("client-01")},
                        {ok, chef_test_suite_helper:make_az_id("client-02")},
                        {ok, chef_test_suite_helper:make_az_id("client-03")}
                    ],
                    ?assertEqual(4, oc_chef_group:handle_error_for_update_ops(OpsResults, 1))
            end
        },
        { "When one result is forbidden",
            fun() ->
                    ForbiddenAuthzId = chef_test_suite_helper:make_az_id("forbidden-01"),
                    OpsResults = [
                        {ok, chef_test_suite_helper:make_az_id("client-01")},
                        {error, forbidden, ForbiddenAuthzId},
                        {ok, chef_test_suite_helper:make_az_id("client-03")}
                    ],
                    Expected = {error, error_in_bifrost},
                    ?assertEqual(Expected, oc_chef_group:handle_error_for_update_ops(OpsResults, 1))
            end
        },
        { "When two results is forbidden",
            fun() ->
                    ForbiddenAuthzId1 = chef_test_suite_helper:make_az_id("forbidden-01"),
                    ForbiddenAuthzId2 = chef_test_suite_helper:make_az_id("forbidden-02"),
                    OpsResults = [
                        {ok, chef_test_suite_helper:make_az_id("client-01")},
                        {error, forbidden, ForbiddenAuthzId1},
                        {error, forbidden, ForbiddenAuthzId2}
                    ],
                    Expected = {error, error_in_bifrost},
                    ?assertEqual(Expected, oc_chef_group:handle_error_for_update_ops(OpsResults, 1))
            end
        },
        { "When one results is forbidden, another is server error",
            fun() ->
                    ForbiddenAuthzId = chef_test_suite_helper:make_az_id("forbidden-01"),
                    OpsResults = [
                        {ok, chef_test_suite_helper:make_az_id("client-01")},
                        {error, forbidden, ForbiddenAuthzId},
                        {error, server_error, chef_test_suite_helper:make_az_id("client-03")}
                    ],
                    Expected = {error, error_in_bifrost},
                    ?assertEqual(Expected, oc_chef_group:handle_error_for_update_ops(OpsResults, 1))
            end
        },
        { "When one result is server error",
            fun() ->
                    ServerErrAuthzId = chef_test_suite_helper:make_az_id("serverr-01"),
                    OpsResults = [
                        {ok, chef_test_suite_helper:make_az_id("client-01")},
                        {error, server_error, ServerErrAuthzId},
                        {ok, chef_test_suite_helper:make_az_id("client-03")}
                    ],
                    Expected = {error, error_in_bifrost},
                    ?assertEqual(Expected, oc_chef_group:handle_error_for_update_ops(OpsResults, 1))
            end
        },
        { "When two results are server errors",
            fun() ->
                    ServerErrAuthzId1 = chef_test_suite_helper:make_az_id("serverr-01"),
                    ServerErrAuthzId2 = chef_test_suite_helper:make_az_id("serverr-02"),
                    OpsResults = [
                        {ok, chef_test_suite_helper:make_az_id("client-01")},
                        {error, server_error, ServerErrAuthzId1},
                        {error, server_error, ServerErrAuthzId2}
                    ],
                    Expected = {error, error_in_bifrost},
                    ?assertEqual(Expected, oc_chef_group:handle_error_for_update_ops(OpsResults, 1))
            end
        }
    ].

