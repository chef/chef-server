
# Set Omnibus Buildkite Plugin Version
ENV['OMNIBUS_BUILDKITE_PLUGIN_VERSION'] = '8291538da3f3ab8f037ff7a56d77e1ab02c4bbf9'

# Disable git caching
# ------------------------------
# use_git_caching false

# Enable S3 asset caching
# ------------------------------
use_s3_caching true
s3_access_key  ENV['AWS_ACCESS_KEY_ID']
s3_secret_key  ENV['AWS_SECRET_ACCESS_KEY']
s3_bucket      'opscode-omnibus-cache'

# Customize compiler bits
# ------------------------------
build_retries 3
fetcher_read_timeout 120

# Load additional software
# ------------------------------
# software_gems ['omnibus-software', 'my-company-software']
# local_software_dirs ['/path/to/local/software']

fatal_transitive_dependency_licensing_warnings true

# Build in FIPS compatability mode
# ------------------------------
fips_mode (ENV['OMNIBUS_FIPS_MODE'] || '').downcase == "true"
