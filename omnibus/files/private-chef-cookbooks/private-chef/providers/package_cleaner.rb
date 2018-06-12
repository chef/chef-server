# Copyright:: 2013-2018 Chef Software, Inc.
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

# Perform safe cleanup after old packages and services following a
# successful Private Chef upgrade.  Executables will have been removed
# from the package upgrade process, but old data, configuration, logs,
# directories, etc. can be left behind.

action :clean do
  remove_service if new_resource.is_service
  remove_files
  remove_links
  remove_users
  remove_groups
  remove_directories
end

def remove_directory(dir)
  directory dir do
    action :delete
    recursive true
  end
end

def remove_directories
  new_resource.directories.each do |dir|
    remove_directory(dir)
  end
end

# Our runit services have a standardized structure; this ensures that
# we completely remove everything.
def remove_service
  unlink "#{new_resource.service_link_root}/#{new_resource.package}"
  unlink "#{new_resource.service_init_link_root}/#{new_resource.package}"
  remove_directory "#{new_resource.service_root}/#{new_resource.package}"
end

def remove_files
  new_resource.files.each do |f|
    file f do
      action :delete
      backup false # We're removing cruft from the system; we don't
                   # want to leave backup cruft
    end
  end
end

def unlink(l)
  link l do
    action :delete
  end
end

def remove_links
  new_resource.links.each do |l|
    unlink(l)
  end
end

def remove_users
  new_resource.users.each do |u|
    user u do
      action :remove
    end
  end
end

def remove_groups
  new_resource.groups.each do |g|
    group g do
      action :remove
    end
  end
end
