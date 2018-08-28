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

-define(user_db, "opscode_account").
-define(auth_join_db, "opscode_account").

%
% Opscode Chef_views.
%
-define(mixlib_auth_client_design,
        "Mixlib::Authorization::Models::Client-fec21b157b76e08b86e92ef7cbc2be81").

-define(mixlib_auth_container_design,
	"Mixlib::Authorization::Models::Container-f6aead5acfa18f649f9f951ad5570324").

-define(mixlib_auth_cookbook_design,
	"Mixlib::Authorization::Models::Cookbook-98638da9d7bfb86c50d448361c5ce691").

-define(mixlib_auth_data_bag_design,
	"Mixlib::Authorization::Models::DataBag-ff80eed5150b3cf59c54630503d83bb5").

-define(mixlib_auth_environment_design,
	"Mixlib::Authorization::Models::Environment-bd1a09cec7a4655bf54f4b98ec5ec156").

-define(mixlib_auth_group_design,
	"Mixlib::Authorization::Models::Group-59a505c964199e318b67910cc642a06").

-define(mixlib_auth_node_design,
	"Mixlib::Authorization::Models::Node-8554f173ac3e9bfa55a0836e19b0f232").

-define(mixlib_auth_role_design,
	"Mixlib::Authorization::Models::Role-793b383e56e849fca5901cd66b92bde7").


-define(client_design, "clients").
-define(cookbook_design, "cookbooks").
-define(data_bag_design, "data_bags").
-define(data_bag_item_design, "data_bag_items").
-define(environment_design, "environments").
-define(node_design, "nodes").
-define(role_design, "roles").


%
% Opscode Account Views
%
-define(mixlib_auth_org_design,
        "Mixlib::Authorization::Models::Organization-eed4ffc4a127815b935ff840706c19de").

-define(mixlib_auth_join_design,
        "Mixlib::Authorization::AuthJoin-25834c5a8d6a9586adb05320f3f725e8").

-define(organization_user_design,
        "OrganizationUser-5c1085b0dd852acf9c74bbfe97f66406").

-define(mixlib_auth_user_design,
        "Mixlib::Authorization::Models::User-e8e718b2cc7860fc5d5beb40adc8511a").
