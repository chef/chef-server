require 'omnibus/s3_cache'

module Omnibus
  class S3Cache
    class << self
      alias_method :original_populate, :populate
      
      def populate
        missing.each do |software|
          without_caching do
            software.fetch
          end

          key     = key_for(software)
          fetcher = software.fetcher

          log.info(log_key) do
            "Caching '#{fetcher.downloaded_file}' to '#{Config.s3_bucket}/#{key}'"
          end

          md5 = digest(fetcher.downloaded_file, :md5)

          File.open(fetcher.downloaded_file, "rb") do |file|
            store_object(key, file, md5, "private")  # Changed to private
          end
        end

        true
      end
    end
  end
end

# Disable git caching
# ------------------------------
# use_git_caching false

# Enable S3 asset caching
# ------------------------------
use_s3_caching true
s3_access_key  ENV['AWS_ACCESS_KEY_ID']
s3_secret_key  ENV['AWS_SECRET_ACCESS_KEY']
s3_bucket      'opscode-omnibus-cache-private'

# Customize compiler bits
# ------------------------------
build_retries 3
fetcher_read_timeout 120

# Load additional software
# ------------------------------
# software_gems ['omnibus-software', 'my-company-software']
# local_software_dirs ['/path/to/local/software']

fatal_transitive_dependency_licensing_warnings true

# use_internal_sources ENV.fetch("OMNIBUS_USE_INTERNAL_SOURCES", true)

# Build in FIPS compatability mode
# ------------------------------
fips_mode (ENV['OMNIBUS_FIPS_MODE'] || '').downcase == "true"
