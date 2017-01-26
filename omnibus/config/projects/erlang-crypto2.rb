name "erlang-crypto2"
maintainer "Chef Software, Inc. <maintainers@chef.io>"
homepage   "https://www.chef.io"
license "Apache-2.0"
license_file "LICENSE"

package_name    "erlang-crypto2"
# replace         "private-chef"
# conflict        "private-chef"
install_dir     "/opt/erlang-crypto2"
build_version   "100.0.0"
build_iteration 1

override :erlang, version: "17.5"

# creates required build directories
dependency "preparation"
dependency "erlang-crypto2"

package :rpm do
  signing_passphrase ENV['OMNIBUS_RPM_SIGNING_PASSPHRASE']
end

exclude "**/.git"
exclude "**/bundler/git"
