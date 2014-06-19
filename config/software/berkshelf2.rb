#
# Copyright:: Copyright (c) 2012-2014 Chef Software, Inc.
# License:: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

name "berkshelf2"
default_version "2.0.17"

dependency "ruby"
dependency "rubygems"
dependency "nokogiri"
dependency "libffi"

build do
  gem 'install hashie --no-rdoc --no-ri -v \'~> 2.0.0\''
  gem 'install varia_model --no-rdoc --no-ri -v 0.3.2'
  gem "install berkshelf --no-rdoc --no-ri -v #{version}"
end
