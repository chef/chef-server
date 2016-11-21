%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% Copyright 2016 Chef Software, Inc. All Rights Reserved.
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

-module(oc_chef_wm_named_organization_tests).

-include("oc_chef_wm.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(READ_ACCESS_GROUP_NAME, "ponyville_read_access_group").
-define(ORG_ID, <<"12341234123412341234123412341234">>).
-define(ORG_NAME, <<"ponyville">>).

delete_resource_test_() ->
  {foreach,
     fun() ->
             meck:new(oc_chef_authz_db, [passthrough]),
             meck:expect(oc_chef_authz_db, fetch_read_access_group, fun(mock_authz_context, ?ORG_NAME) ->
	     								    #oc_chef_group{name=?READ_ACCESS_GROUP_NAME}
	     							    end),
             meck:new(oc_chef_object_db),
             meck:expect(oc_chef_object_db, safe_delete, fun(mock_db_context, Group, _RequestorId) ->
								 case Group#oc_chef_group.name of
								     ?READ_ACCESS_GROUP_NAME -> true;
								     _ -> throw(safe_delete_spec_failed)
								 end
							 end),
	     meck:new(oc_chef_wm_base),
	     meck:expect(oc_chef_wm_base, delete_object, fun(mock_db_context, mock_oc_chef_organization, mock_requestor_id) ->
	     							 ok
	     						 end),
	     meck:new(oc_chef_organization),
	     meck:expect(oc_chef_organization, assemble_organization_ejson, fun(mock_oc_chef_organization) ->
	     									    mock_json_response
	     								    end),
	     meck:new(chef_wm_util),
	     meck:expect(chef_wm_util, set_json_body, fun(#wm_reqdata{}, mock_json_response) ->
							      mock_final_req_body
						      end)
     end,
     fun(_) ->
             meck:unload()
     end,
     [
      {"a global group when not a member returns true (forbidden)",
       fun() ->
               Req = make_req_data(),
               State = make_base_state(),
               Result = oc_chef_wm_named_organization:delete_resource(Req, State),
	       ?assert(meck:validate(oc_chef_authz_db)),
	       ?assert(meck:validate(oc_chef_object_db)),
	       ?assert(meck:validate(oc_chef_wm_base)),
	       ?assert(meck:validate(oc_chef_organization)),
	       ?assert(meck:validate(chef_wm_util)),
               ?assertEqual({true, mock_final_req_body, State}, Result)
       end}
     ]}.

make_req_data() ->
    #wm_reqdata{}.

make_base_state() ->
    #base_state{
       organization_name = ?ORG_NAME,
       organization_guid = ?ORG_ID,
       auth_skew = 900,
       chef_authz_context = mock_authz_context,
       chef_db_context = mock_db_context,
       requestor_id = mock_requestor_id,
       resource_state = #organization_state{
			   oc_chef_organization = mock_oc_chef_organization
			  }
      }.
