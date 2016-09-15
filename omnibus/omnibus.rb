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

# This is temporarily disabled due to a high number of failures
# while trying to resolve sqitch licensing due to the cpan
# service timing out. .  We'll re-enable it
# once we have a process available that lets us declare
# component licensing outside of the build itself.
fatal_transitive_dependency_licensing_warnings false
