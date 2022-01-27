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

# That's right folks, there are no properties.

# NOTE: Uses the value of certain node attributes in the course of execution.
#
# * node['private_chef']['postgresql']['data_dir']: the PostgreSQL
#   data directory on the the current Chef run
# * node['private_chef']['postgresql']['pg_upgrade_timeout']: the
#   maximum runtime for pg_upgrade
# * node['private_chef']['postgresql']['username']: the user the
#   pg_upgrade process is run as
# * node['private_chef']['postgresql']['version']: the major version
#   of the current installed PostgreSQL release
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
# segregated by postgres major version.
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

provides :pg_upgrade

action :upgrade do
  if rename_parent_required?
    # Only rename the parent of the data directory
    converge_by('Renaming database cluster parent directory') do
      shutdown_postgres
      rename_existing_cluster_parent
    end
  elsif move_required?
    # Move the data directory into a new parent directory
    converge_by('Moving database cluster data directory') do
      check_required_disk_space unless ENV['CS_SKIP_PG_DISK_CHECK'] == '1'
      shutdown_postgres
      move_existing_cluster
    end
  elsif upgrade_required?
    # Engage pg_upgrade to do data migration
    converge_by('Upgrading database cluster') do
      check_required_disk_space unless ENV['CS_SKIP_PG_DISK_CHECK'] == '1'
      shutdown_postgres
      initialize_new_cluster
      update_to_latest_version
    end
  end
end

action_class do
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

  def parent_dir(dir)
    return if dir.nil?
    ::File.expand_path("#{dir}/..")
  end

  def mountpoint?(dir)
    stat1 = ::File.new(dir).lstat
    stat2 = ::File.new(::File.expand_path("#{dir}/..")).lstat
    stat1.dev != stat2.dev || stat1.ino == stat2.ino
  rescue Errno::ENOENT
    false
  end

  # If this file exists, assume that the upgrade has succeeded
  def sentinel_file
    ::File.join(new_data_dir, 'upgraded.sentinel')
  end

  # @return [Boolean] Whether or not a cluster rename is needed, and the
  # why-run message to describe what we're doing (or why we're not doing
  # anything)
  def rename_parent_required?
    # Check if we can simply rename the data directory's parent
    # directory, so we don't have to concern ourselves with actual
    # data copying or migration
    Chef::Log.debug 'Checking if the database cluster parent directory needs to be renamed'

    # Get the parents and grandparents of the data directories
    old_data_parent = parent_dir(old_data_dir)
    new_data_parent = parent_dir(new_data_dir)
    old_data_grandparent = parent_dir(old_data_parent)
    new_data_grandparent = parent_dir(new_data_parent)

    if old_data_dir.nil?
      # This will only happen if we've never successfully completed a
      # Private Chef installation on this machine before, or if the existing
      # database cluster is already in the correct location
      Chef::Log.debug 'No old database cluster detected; nothing to rename'
      false
    elsif ::File.exist?(new_data_parent)
      # If the new parent directory already exists, we cannot rename the
      # old parent directory to an existing path
      Chef::Log.debug 'New parent directory already exists; nothing to rename'
      false
    elsif mountpoint?(old_data_parent)
      # If the old parent directory is a mountpoint, we cannot rename the
      # old parent directory
      Chef::Log.debug 'Old parent directory is a mountpoint; nothing to rename'
      false
    elsif old_data_grandparent != new_data_grandparent
      # If the parent directory will be renamed, the old and new parent
      # directories cannot have different filesystem paths. This should
      # never happen, but check anyways
      Chef::Log.debug 'Old and new parent directories exist at different filesystem paths; nothing to rename'
      false
    else
      # Hmm, looks like we need to rename directories after all
      Chef::Log.debug 'Database cluster parent directory rename is required'
      true
    end
  end

  # @return [Boolean] Whether or not a cluster move is needed, and the
  # why-run message to describe what we're doing (or why we're not doing
  # anything)
  def move_required?
    # Since we cannot simply rename the directory, for reasons...
    # We instead attempt a filesystem move of the data directory into a new
    # parent. If we're doing this, there's probably something weird going
    # on, but if this doesn't fix things, we can still pg_upgrade.
    Chef::Log.debug 'Checking if the database cluster data directory needs to move'

    if old_data_dir.nil?
      # This will only happen if we've never successfully completed a
      # Private Chef installation on this machine before, or if the existing
      # database cluster is already in the correct location
      Chef::Log.debug 'No old database cluster detected; nothing to move'
      false
    elsif Dir.exist?(new_data_dir)
      # If the new data directory already exists, we shouldn't consider
      # overwriting the data directory. Maybe it was just initialized,
      # and needs to finish the pg_upgrade process
      Chef::Log.debug 'Database cluster data directory already exists from previous installation; nothing to move'
      false
    elsif version_from_data_dir(old_data_dir) != node['private_chef']['postgresql']['version']
      # If the major version from the old PG_VERSION file doesn't match
      # our target major version, we cannot simply move the directories
      # around, so we avoid doing anything here
      Chef::Log.debug 'Database cluster major versions differ, may need upgrading instead; nothing to move'
      false
    else
      # Hmm, looks like we need to move directories after all
      Chef::Log.debug 'Database cluster data directory move is required'
      true
    end
  end

  # @return [Boolean] Whether or not an upgrade is needed, and the
  # why-run message to describe what we're doing (or why we're not doing
  # anything)
  def upgrade_required?
    # The data directory could not be renamed or moved, so check if
    # pg_upgrade should migrate the database cluster
    Chef::Log.debug 'Checking if the database cluster needs to be upgraded'

    if old_data_dir.nil?
      # This will only happen if we've never successfully completed a
      # Private Chef installation on this machine before, or if the existing
      # database cluster is already in the correct location
      Chef::Log.debug 'No old database cluster detected; nothing to upgrade'
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
      Chef::Log.debug 'Database cluster upgrade is required'
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

    notify_group 'Shutting down PostgreSQL for update' do
      notifies :stop, 'component_runit_service[postgresql]', :immediately
      action :run
    end
  end

  def initialize_new_cluster
    pg_cluster new_data_dir
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

        # the version file contains a single line with the major version
        # (e.g. "9.6\n", "13\n")
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
  #   e.g. "9.6", "13".  Note that this does NOT include minor release
  #   levels, like "9.6.22" or "13.4"
  # @return [String] the absolute path to the binaries.
  def binary_path_for(version)
    "/opt/opscode/embedded/postgresql/#{version}/bin"
  end

  def rename_existing_cluster_parent
    # Get the parents of the data directories
    old_data_dir_parent = parent_dir(old_data_dir)
    new_data_dir_parent = parent_dir(new_data_dir)

    ::File.rename(old_data_dir_parent, new_data_dir_parent)
    Chef::Log.debug("Renamed #{old_data_dir_parent} to #{new_data_dir_parent}")
  end

  def move_existing_cluster
    # Hold for logging
    old_dir = old_data_dir

    # Get the parents of the data directories
    old_data_dir_parent = parent_dir(old_data_dir)
    new_data_dir_parent = parent_dir(new_data_dir)
    new_owner = node['private_chef']['postgresql']['username']

    # Create the destination parent, if needed
    unless ::File.exist?(new_data_dir_parent)
      ::Dir.mkdir(new_data_dir_parent, 0750)
      ::FileUtils.chown(new_owner, new_owner, new_data_dir_parent)
      Chef::Log.debug("Created new data destination parent directory #{new_data_dir_parent}")
    end

    # Move the old data directory into the new destination parent
    ::FileUtils.mv(old_dir, new_data_dir_parent)
    Chef::Log.debug("Moved #{old_dir} into destination #{new_data_dir_parent}")

    # Remove the old parent directory
    if ::Dir.empty?(old_data_dir_parent)
      ::Dir.rmdir(old_data_dir_parent)
      Chef::Log.debug("Removed old directory #{old_data_dir_parent}")
    end
  end

  def update_to_latest_version
    execute 'upgrade_postgres_cluster' do
      command lazy {
        old_version = version_from_data_dir(old_data_dir)

        # The new data directory may have just been created; that's why
        # this needs to be evaluated lazily

        new_version = version_from_data_dir(new_data_dir)

        old_bins = binary_path_for(old_version)
        new_bins = binary_path_for(new_version)

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
end
