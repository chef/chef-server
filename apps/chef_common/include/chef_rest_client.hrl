%
% License:: Apache License, Version 2.0
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
% @author Mark Anderson <mark@opscode.com>
% @author Kevin Smith <kevin@opscode.com>
% @copyright Copyright 2011 Opscode, Inc.
% @version 0.0.1
% @doc Helper module for calling various Chef REST endpoints
% @end

-record(chef_rest_client, {base_url :: string(),
                           user_name :: string(),
                           private_key :: binary(),
                           request_source = user :: 'user' | 'web' }).

