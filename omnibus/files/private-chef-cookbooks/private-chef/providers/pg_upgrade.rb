#
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

# NOTE:
#
# Uses the value of certain node attributes in the course of execution.
#
# * node['previous_run']['postgresql']['data_dir']: the PostgreSQL
#   data directory on the last Chef run
# * node['private_chef']['postgresql']['data_dir']: the PostgreSQL
#   data directory on the the current Chef run
# * node['private_chef']['postgresql']['username']: the user the
#   pg_upgrade process is run as
#
# Assumes that binaries are stored in
# /opt/opscode/embedded/postgresql/$VERSION/bin.

# ASSUMPTIONS
#
# Fresh Install: -------------------------------------------------
#
# There will not be any record of an old data directory.  In this
# case, we should bail out entirely, as there is really nothing to
# upgrade
#
# Existing Install: ----------------------------------------------
#
# There is an old data directory specified, and it is different from
# the new data directory:
#
# If there is an old data directory, then we can safely assume that
# there is a valid Postgres cluster initialized there (since a Chef
# run must have succeeded for us to see any old values.)  If the new
# value for the data directory is the same as the old one, then we
# don't need to do anything, since we are keeping our data directories
# segregated by postgres version.
#
# If the new value is different, however, two things may be happening:
# one, the user may have gone and moved their existing data directory
# somewhere else.  In that case, we should expect to find an existing
# cluster there.  In that case, we'll assume that they know what
# they're doing and not proceed further.
#
# On the other hand, this may be a legitimate upgrade case.  If the
# new directory does not exist, or does exist, but doesn't have an
# initialized cluster, we probably need to update.  Since cluster
# creation and updating are separate steps (and we could conceivably
# fail between them), we're going to drop off a sentinel file that we
# can check to see if we've been migrated.

action :upgrade do
  if upgrade_required?
    converge_by('Upgrading database cluster') do
      check_required_disk_space unless ENV['CS_SKIP_PG_DISK_CHECK'] == '1'
      shutdown_postgres
      initialize_new_cluster
      update_to_latest_version
    end
  end
end

def old_data_dir
  # Something to play with - just discover the first initialized data dir
  # that isn't the new data dir.
  # Assumption: we will not support installing multiple versions of this package
  # on top of each other without expecting at least one reconfigure in between...
  #
  # Cheat and use gem's version parsing and comparison operators.
  # Note we're doing a reverse sort to put the highest version at 0,
  # and we're not checking '==' because we can't have two paths of the same name.
  ::Dir.glob(::File.join(::File.expand_path('../..', new_data_dir), '*/data'))
       .reject { |dir| dir == new_data_dir } # ignore the new one
       .map { |dir| [dir, version_from_data_dir(dir)] }
       .reject { |_dir, vsn| !vsn } # filter those with no PG_VERSION
       .sort { |a, b| Gem::Version.new(a[1]) > Gem::Version.new(b[1]) ? -1 : 1 }
       .map(&:first) # drop the versions again
       .first
end

def new_data_dir
  node['private_chef']['postgresql']['data_dir']
end

# If this file exists, assume that the upgrade has succeeded
def sentinel_file
  ::File.join(new_data_dir, 'upgraded.sentinel')
end

# @return [Boolean] Whether or not an upgrade is needed, and the
# why-run message to describe what we're doing (or why we're not doing
# anything)
def upgrade_required?
  if old_data_dir.nil?
    # This will only happen if we've never successfully completed a
    # Private Chef installation on this machine before.  In that case,
    # there is (by definition) nothing to upgrade
    Chef::Log.debug 'No prior database cluster detected; nothing to upgrade'
    false
  elsif old_data_dir == new_data_dir
    # If the directories are the same, then we're not changing anything
    # (since we keep data directories in version-scoped
    # directories); i.e., this is just another garden-variety chef run
    Chef::Log.debug 'Database cluster is unchanged; nothing to upgrade'
    false
  elsif Dir.exist?(new_data_dir) &&
        cluster_initialized?(new_data_dir) &&
        ::File.exist?(sentinel_file)
    # If the directories are different, we may need to do an upgrade,
    # but only if all the steps along the way haven't been completed
    # yet.  We'll look for a sentinel file (which we'll write out
    # following a successful upgrade) as final confirmation.
    #
    # If we then make it all the way through the chef run, then the next
    # time through, the old_data_dir will be the same as our
    # new_data_dir
    Chef::Log.debug 'Database cluster already upgraded from previous installation; nothing to do'
    false
  else
    # Hmm, looks like we need to upgrade after all
    true
  end
end

#
# Since we don't use the --link flag, we need to ensure the disk has
# enough space for another copy of the postgresql data.
#
def check_required_disk_space
  old_data_dir_size = Du.du(old_data_dir)
  # new_data_dir might not exist at the point of making this check.
  # In that case check the first existing directory above it.
  new_dir = dir_or_existing_parent(new_data_dir)
  free_disk_space = Statfs.new(new_dir).free_space

  if old_data_dir_size < (free_disk_space * 0.90)
    Chef::Log.debug("Old data dir size: #{old_data_dir_size}")
    Chef::Log.debug("  Free disk space: #{free_disk_space}")
    Chef::Log.debug('Free space is sufficient to start upgrade')
    true
  else
    Chef::Log.fatal('Insufficient free space on disk to complete upgrade.')
    Chef::Log.fatal("The current postgresql data directory contains #{old_data_dir_size} KB of data but only #{free_disk_space} KB is available on disk.")
    Chef::Log.fatal("The upgrade process requires at least #{old_data_dir_size / 0.90} KB.")
    raise 'Insufficient Disk Space to Upgrade'
  end
end

def dir_or_existing_parent(dir)
  return dir if ::File.exist?(dir)
  return dir if ::File.expand_path(dir) == '/'

  dir_or_existing_parent(::File.expand_path("#{dir}/.."))
end

# If a pre-existing postgres service exists it will need to be shut
# down prior to running the upgrade step.
def shutdown_postgres
  component_runit_service 'postgresql' do
    action :nothing # can this just be 'action :stop'?
  end

  log 'Shutting down PostgreSQL for update' do
    notifies :stop, 'component_runit_service[postgresql]', :immediately
  end
end

def initialize_new_cluster
  private_chef_pg_cluster new_data_dir
end

# Use the existence of a PG_VERSION file in a cluster's data directory
# as an indicator of it having been already set up.
def cluster_initialized?(data_dir)
  ::File.exist?(version_file_for(data_dir))
end

def version_file_for(data_dir)
  ::File.join(data_dir, 'PG_VERSION')
end

# Postgres stores version information inside a cluster's data
# directory; given the directory, then, we can figure out what version
# of Postgres is managing it.
#
# @param data_dir [String] the absolute path of a Postgres cluster's
#   data directory
#
# @return [String, nil] the major version of the Postgres cluster in
#   `data_dir`, or `nil` if the directory does not exist, or if a
#   cluster has not yet been initialized in it
def version_from_data_dir(data_dir)
  if Dir.exist?(data_dir)
    if cluster_initialized?(data_dir)
      # Might not be initialized yet if a prior Chef run failed between
      # creating the directory and initializing a cluster in it

      # the version file contains is a single line with the version
      # (e.g. "9.2\n")
      IO.read(version_file_for(data_dir)).strip
    end
  end
end

# Find the location of the binaries that interact with a Postgres
# cluster of the given `version`
#
# @note The path used in this method are taken from how we currently
#   use Postgres software definitions.
#
# @param version [String] indicates the major release of Postgres,
#   e.g. "9.1", "9.2".  Note that this does NOT include patch-levels,
#   like "9.1.9" or "9.2.4"
# @return [String] the absolute path to the binaries.
def binary_path_for(version)
  "/opt/opscode/embedded/postgresql/#{version}/bin"
end

def update_to_latest_version
  execute 'upgrade_postgres_cluster' do
    command lazy {
      old_version = version_from_data_dir(old_data_dir)

      # The new data directory may have just been created; that's why
      # this needs to be evaluated lazily
      new_version = version_from_data_dir(new_data_dir)

      old_bins = binary_path_for(old_version)
new_bins = binary_path_for(new_version.to_i < 12 ? new_version : node['private_chef']['postgresql']['version'])

      <<-EOM.gsub(/\s+/, ' ').strip!
      #{new_bins}/pg_upgrade
        --old-datadir=#{old_data_dir}
        --new-datadir=#{new_data_dir}
        --old-bindir=#{old_bins}
        --new-bindir=#{new_bins}
        --old-options=" -c config_file=#{::File.join(old_data_dir, 'postgresql.conf')}"
        --new-options=" -c config_file=#{::File.join(new_data_dir, 'postgresql.conf')}"
      && date > #{sentinel_file}
    EOM
    }
    user node['private_chef']['postgresql']['username']
    cwd new_data_dir # TODO: Should this be some other directory, instead?
    creates sentinel_file
    timeout node['private_chef']['postgresql']['pg_upgrade_timeout']
  end
end
