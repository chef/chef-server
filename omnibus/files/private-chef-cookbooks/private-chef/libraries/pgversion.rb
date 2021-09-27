# frozen_string_literal: true
#
##
# PostgreSQL has a slightly complicated way of representing server versions over
# the course of its history, in particular, what is considered a major version.
# The full version is reflected in the read-only configuration parameter
# server_version.
#
# This major version is reflected in the PG_VERSION file in the top level of the
# PostgreSQL data directory. In theory, the data format on-disk changes only
# with major releases.
#
# Additionally, since PostgreSQL 8.2, the version is available in integer
# format, stored in the read-only configuration parameter server_version_num.
# This is intended to be a machine-readable and consistent format for reliable
# version comparisons.
#
# Before version 10, the first two values are considered 'major':
#   server_version = major1 . major2 . minor
#   server_version_num = ( '%d%02d%02d' <- major1, major2, minor )
#
# After version 10, only the first value is considered 'major':
#   server_version = major . minor
#   server_version_num = ( '%d%04d' <- major, minor )
#
# Examples:
#
#   server_version = 9.6.22 :
#     - has major version of '9.6'
#     - has minor version of '22'
#     - has server_version_num value of 90622
#     - PG_VERSION file contains '9.6'
#
#   server_version = 13.4 :
#     - has major version of '13'
#     - has minor version of '4'
#     - has server_version_num value of 130004
#     - PG_VERSION file contains '13'
#
# Ref:
# http://www.databasesoup.com/2016/05/changing-postgresql-version-numbering.html

class PgVersion < Gem::Version
  def major
    @major ||=
      begin
        segments = self.segments
        if segments[0].to_i >= 10
          self.class.new segments[0].to_s
        else
          self.class.new segments[0..1].join('.')
        end
      end
  end

  # We override here to also accept the server_version_num value, and coerce the
  # value into a proper dotted version string.
  def self.new(input)
    if input.is_a?(self)
      super
    elsif input.to_i >= 100000
      new input.to_s.match(/([0-9]+)([0-9]{4})$/).captures.map(&:to_i).join('.')
    elsif input.to_i >= 80200
      new input.to_s.match(/([0-9]+)([0-9]{2})([0-9]{2})$/).captures.map(&:to_i).join('.')
    else
      super
    end
  end

  # Return the version in server_version_num integer format, if server_version
  # is at or greater than 8.2, otherwise, zero.
  def to_i
    segments = self.segments
    if segments[0].to_i >= 10
      format('%<major>d%<minor>04d',
             major: segments[0].to_i,
             minor: segments[1].to_i
            ).to_i
    elsif segments[0].to_i == 9 || (segments[0].to_i == 8 && segments[1].to_i >= 2)
      format('%<major1>d%<major2>02d%<minor>02d',
             major1: segments[0].to_i,
             major2: segments[1].to_i,
             minor: segments[2].to_i
            ).to_i
    else
      nil.to_i
    end
  end
end
