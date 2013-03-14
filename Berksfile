chef_api :config
site :opscode

# If you have no intention of setting up a metrics server for testing,
# and don't want to have to have the opscode-dev-vm repo with its
# cookbooks on your machine, you can set this to false
ENABLE_METRICS_SERVER = true

if ENV['OPSCODE_SRC'].nil?
  puts "ERROR: please export OPSCODE_SRC variable."
  exit 1
end
if ENV['OPSCODE_COOKBOOKS'].nil?
  puts "ERROR: please export OPSCODE_COOKBOOKS variable."
  exit 1
end

cookbook "opscode-heimdall", git: "git@github.com:opscode-cookbooks/opscode-heimdall.git"
# cookbook "opscode-heimdall", path: "#{ENV['OPSCODE_COOKBOOKS']}/opscode-heimdall"
cookbook "opscode-pedant", git: "git@github.com:opscode-cookbooks/opscode-pedant.git"
# cookbook "opscode-pedant", path: "#{ENV['OPSCODE_COOKBOOKS']}/opscode-pedant"
cookbook "opscode-ruby", git: "git@github.com:opscode-cookbooks/opscode-ruby.git"
# cookbook "opscode-ruby", path: "#{ENV['OPSCODE_COOKBOOKS']}/opscode-ruby"

# This is only needed for local dev work
cookbook "opscode-dev-shim", git: "git@github.com:opscode-cookbooks/opscode-dev-shim.git"
# cookbook "opscode-dev-shim", path: "#{ENV['OPSCODE_COOKBOOKS']}/opscode-dev-shim"

if ENABLE_METRICS_SERVER
  # These are currently needed if you want to get graphite running locally
  DEV_VM_COOKBOOK_DIR = "#{ENV['OPSCODE_SRC']}/opscode-dev-vm/cookbooks"
  cookbook "piab",     path: "#{DEV_VM_COOKBOOK_DIR}/piab"
  cookbook "estatsd",  path: "#{DEV_VM_COOKBOOK_DIR}/estatsd"
  cookbook "graphite", path: "#{DEV_VM_COOKBOOK_DIR}/graphite"
  cookbook "gunicorn", path: "#{DEV_VM_COOKBOOK_DIR}/gunicorn"
end
