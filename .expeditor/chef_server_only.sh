#!/bin/bash -e

#===============================================================================
#Downloading the automate repo
#===============================================================================

export DEFAULT_AUTOMATE_REPO="https://github.com/chef/automate-private.git"

if [ -z "$AUTOMATE_REPO" ];
then
AUTOMATE_REPO="$DEFAULT_AUTOMATE_REPO"
fi

git clone $AUTOMATE_REPO

repo_dir=`basename -s .git $AUTOMATE_REPO`

cd ${repo_dir}

if [ "${AUTOMATE_BRANCH}" != "" ]
then
  git checkout "${AUTOMATE_BRANCH}"
fi

#===============================================================================

# Add Ruby environment debugging before do_test_deploy
cat > integration/ruby_debug.rb << 'RUBY'
begin
  require 'rbconfig'
  require 'rubygems'
  $stderr.puts "[RUBY-DEBUG] Ruby executable: #{RbConfig.ruby}"
  $stderr.puts "[RUBY-DEBUG] Ruby version: #{RUBY_VERSION}"
  $stderr.puts "[RUBY-DEBUG] RubyGems version: #{Gem::VERSION}"
  $stderr.puts "[RUBY-DEBUG] GEM_HOME: #{ENV['GEM_HOME']}"
  $stderr.puts "[RUBY-DEBUG] GEM_PATH: #{ENV['GEM_PATH']}"
  $stderr.puts "[RUBY-DEBUG] BUNDLE_GEMFILE: #{ENV['BUNDLE_GEMFILE']}"
  $stderr.puts "[RUBY-DEBUG] BUNDLE_PATH: #{ENV['BUNDLE_PATH']}"
  $stderr.puts "[RUBY-DEBUG] RUBYLIB: #{ENV['RUBYLIB']}"
  $stderr.puts "[RUBY-DEBUG] Load path (first 20 entries):"
  $LOAD_PATH.first(20).each_with_index { |path, i| $stderr.puts "  #{i}: #{path}" }
  bundler_version = begin
    require 'bundler'
    Bundler::VERSION
  rescue LoadError => e
    "LoadError: #{e.message}"
  end
  $stderr.puts "[RUBY-DEBUG] Bundler version: #{bundler_version}"
rescue => e
  $stderr.puts "[RUBY-DEBUG] Error during debug: #{e.class}: #{e.message}"
end
RUBY

# Inject debug into do_test_deploy function
if [ -f ./integration/tests/chef_server_only.sh ]; then
  # Insert debug right after the do_test_deploy function starts
  sed -i '/^[[:space:]]*do_test_deploy[[:space:]]*()[[:space:]]*{/a \
echo "[RUBY-DEBUG] Entering do_test_deploy function" 1>&2\
ruby integration/ruby_debug.rb' ./integration/tests/chef_server_only.sh
fi

chmod +x ./integration/tests/chef_server_only.sh
#running the chef_server_only.sh script from the automate repo
integration/run_test integration/tests/chef_server_only.sh
