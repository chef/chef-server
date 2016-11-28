# Copyright: Copyright (c) 2012 Opscode, Inc.
# License: Apache License, Version 2.0
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

module Pedant
  module Utility
    require 'digest'
    require 'base64'
    require 'tempfile'

    # This is equivalent to what Chef itself does internally for
    # checksumming cookbook files to go into a sandbox
    def self.checksum(file)
      Digest::MD5.file(file).hexdigest
    end

    def self.base64_checksum(checksum)
      Base64.encode64([checksum].pack("H*")).strip
    end

    # Generates a a string with the given prefix and a unique suffix
    # incorporating seconds (and nanoseconds) from the epoch, along
    # with the current process id
    #
    # DEPRECATED: Do not use this within the spec. Use pedant_suffix or unique_suffix instead
    #
    # TODO: This should be refactored to merge with the unique_suffix and
    # platform.pedant_run_timestamp. However, to access this, this module needs
    # to know the platform. Not sure how to organize this yet.
    #
    def self.with_unique_suffix(prefix)
      t = Time.now.utc
      "#{prefix}-#{t.to_i}-#{t.nsec}-#{Process.pid}"
    end

    # Generates a temporary File with random content.  Used for testing file
    # uploads via sandboxes
    def self.new_random_file
      f = Tempfile.open("pedant_file")
      f.write(with_unique_suffix("pedant_file_content"))
      f.flush
      f
    end

    # Temporary file with specified content
    def self.new_temp_file(content)
      f = Tempfile.open("pedant_file")
      f.write(content)
      f.flush
      f
    end

    def self.fixture_path(path, &blk)
      Pedant::Gem.fixture_directories.each do |fixture_dir|
        candidate_path = File.expand_path(File.join(fixture_dir, path))
        next unless File.exists?(candidate_path)

        if blk
          return yield candidate_path
        else
          return candidate_path
        end
      end
      return nil # Cannot find the fixture. Raise an error?
    end

    # Returns a File object for the file located at
    # fixtures/$filename
    def self.get_pedant_file(filename, &blk)
      fixture_path(filename) do |candidate_file|
        return File.open(candidate_file)
      end
      return nil # Cannot find the fixture. Raise an error?
    end
  end
end
