#!/usr/bin/env ruby

require 'rubygems'
require 'optparse'
require 'mixlib/shellout'

STDOUT.sync = true
shellout_opts = {:timeout => 1200, :live_stream => STDOUT}

#
# Usage: release.rb --bucket BUCKET
#

options = {}
optparse = OptionParser.new do |opts|
  opts.banner = "Usage: #{$0} [options]"

  opts.on("-b", "--bucket BUCKET", "the name of the S3 bucket to release to") do |bucket|
    options[:bucket] = bucket
  end

  opts.on("-v", "--version VERSION", "the version of the installer to release") do |version|
    options[:version] = version
  end
end

begin
  optparse.parse!
  required = [:bucket, :version]
  missing = required.select {|param| options[param].nil?}
  if !missing.empty?
    puts "Missing required options: #{missing.join(', ')}"
    puts optparse
    exit 1
  end
rescue OptionParser::InvalidOption, OptionParser::MissingArgument
  puts $!.to_s
  puts optparse
  exit 1
end

#
# == Jenkins Build Support Matrix
#
# :key:   - the jenkins build name
# :value: - an Array of Arrays indicating the builds supported by the
#           build. by convention, the first element in the array
#           references the build itself.
#

jenkins_build_support = {
  "os=centos6,role=opc-builder" => [["el", "5", "x86_64"]],
  "os=centos5,role=opc-builder" => [["el", "6", "x86_64"]],
  "os=ubuntu-10-04,role=opc-builder" => [["ubuntu", "10.04", "x86_64"]],
  "os=ubuntu-11-04,role=opc-builder" => [["ubuntu", "11.04", "x86_64"]]
}

# fetch the list of local builds
local_packages = Dir["**/pkg/*"]

# upload the packages and the json metadata
build_support_json = {}
jenkins_build_support.each do |(build, supported_platforms)|
  build_platform = supported_platforms.first

  # find the build in the local packages
  build_package = local_packages.find {|b| b.include?(build)}
  raise unless build_package

  # upload build to build platform directory
  build_location = "/#{build_platform.join('/')}/#{build_package.split('/').last}"
  puts "UPLOAD: #{build_package} -> #{build_location}"

  s3_cmd = ["s3cmd",
            "put",
            "--progress",
            build_package,
            "s3://#{options[:bucket]}#{build_location}"].join(" ")
  shell = Mixlib::ShellOut.new(s3_cmd, shellout_opts)
  shell.run_command
  shell.error!

  # update json with build information
  supported_platforms.each do |(platform, platform_version, machine_architecture)|
    build_support_json[platform] ||= {}
    build_support_json[platform][platform_version] ||= {}
    build_support_json[platform][platform_version][machine_architecture] = {}
    build_support_json[platform][platform_version][machine_architecture][options[:version]] = build_location
  end
end

File.open("platform-support.json", "w") {|f| f.puts JSON.pretty_generate(build_support_json)}

s3_location = "s3://#{options[:bucket]}/opc-platform-support/#{options[:version]}.json"
puts "UPLOAD: platform-support.json -> #{s3_location}"
s3_cmd = ["s3cmd",
          "put",
          "platform-support.json",
          s3_location].join(" ")
shell = Mixlib::ShellOut.new(s3_cmd, shellout_opts)
shell.run_command
shell.error!
