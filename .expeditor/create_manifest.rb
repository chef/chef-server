#!/usr/bin/env ruby

# By default, this script creates a manifest.json file that contains all the packages in the unstable channel.

require 'date'
require 'net/http'
require 'json'
require 'openssl'

BLDR_API_HOST="bldr.habitat.sh"
BLDR_API_USER_AGENT="Chef Expeditor"

def get_latest(channel, origin, name)
  http = Net::HTTP.new(BLDR_API_HOST, 443)
  http.use_ssl = true
  http.verify_mode = OpenSSL::SSL::VERIFY_PEER
  req = Net::HTTP::Get.new("/v1/depot/channels/#{origin}/#{channel}/pkgs/#{name}/latest", {'User-Agent' => BLDR_API_USER_AGENT})
  response = http.request(req)
  latest_release = JSON.parse(response.body)
  latest_release["ident"]
rescue
  puts "ERROR: Could not find latest release for #{origin}/#{name} in #{channel}"
  exit 1
end

def get_hab_deps_latest()
  ret = {}
  ["hab", "hab-sup", "hab-launcher"].each do |name|
    d = get_latest("stable", "core", name)
    ret[name] = "#{d["origin"]}/#{d["name"]}/#{d["version"]}/#{d["release"]}"
  end
  ret
end

version = ENV["VERSION"] || File.read("VERSION").chomp
filename = ENV["VERSION"] || "manifest"

manifest = {}

# The version of the manifest schema - might need to be bumped in the future
manifest["schema_version"] = "1"

# The version of the manifest - the "engineering" version
manifest["build"] = version

# Grab the version of various Habitat components from the deployment-service

hab_deps = get_hab_deps_latest
manifest["hab"] = []
manifest["hab"] << hab_deps["hab"]
manifest["hab"] << hab_deps["hab-sup"]
manifest["hab"] << hab_deps["hab-launcher"]

# Grab the version of hab in the build environment. Comes out in the
# form of 'hab 0.54.0/20180221020527'
hab_version = /(\d+\.\d+\.\d+\/\d{14})/.match(`hab --version`.strip)[0]
manifest["hab_build"] = "core/hab/#{hab_version}"

# Grab the git SHA
manifest["git_sha"] = `git show-ref HEAD --hash`.strip

manifest["packages"] = []

pkg_origin = "chef"

%W{
  openresty-noroot
  oc_id
  #{Chef::Dist::Server::SHORT}-nginx
  bookshelf
  #{Chef::Dist::Server::CTL}
  oc_bifrost
  oc_erchef
}.each do |pkg_name|
  latest_release = get_latest("unstable", pkg_origin, pkg_name)

  pkg_version = latest_release["version"]
  pkg_release = latest_release["release"]

  puts "  Adding package #{pkg_origin}/#{pkg_name}/#{pkg_version}/#{pkg_release}"
  manifest["packages"] << "#{pkg_origin}/#{pkg_name}/#{pkg_version}/#{pkg_release}"
end

manifest["packages"].uniq!
# Sort the packages for easier diff-ing
manifest["packages"].sort!

File.open("#{filename}.json", "w") { |file| file.write(JSON.pretty_generate(manifest)) }
