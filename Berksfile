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
  puts """
  ERROR: please export OPSCODE_COOKBOOKS variable.

  This should be a directory with checkouts of cookbooks from the
  'opscode-cookbooks' Github organization.

  If you point it to anything else (like your checkout of the
  opscode/opscode-platform-cookbooks repo), you are probably going
  to be unhappy.
  """
  exit 1
end

cookbook "opscode-bifrost", git: "git@github.com:opscode-cookbooks/opscode-bifrost.git"
# cookbook "opscode-bifrost", path: "#{ENV['OPSCODE_COOKBOOKS']}/opscode-bifrost"
cookbook "opscode-pedant", git: "git@github.com:opscode-cookbooks/opscode-pedant.git"
# cookbook "opscode-pedant", path: "#{ENV['OPSCODE_COOKBOOKS']}/opscode-pedant"
cookbook "opscode-ruby", git: "git@github.com:opscode-cookbooks/opscode-ruby.git"
# cookbook "opscode-ruby", path: "#{ENV['OPSCODE_COOKBOOKS']}/opscode-ruby"
cookbook "sqitch", git: "git@github.com:opscode-cookbooks/sqitch.git"
# cookbook "sqitch", path: "#{ENV['OPSCODE_COOKBOOKS']}/sqitch"

# This is only needed for local dev work
cookbook "opscode-dev-shim", git: "git@github.com:opscode-cookbooks/opscode-dev-shim.git"
# cookbook "opscode-dev-shim", path: "#{ENV['OPSCODE_COOKBOOKS']}/opscode-dev-shim"

# TODO recent versions of opscode-postgresql don't work for local dev--fix that and remove this!
cookbook "opscode-postgresql", '= 0.1.3'

if ENABLE_METRICS_SERVER
  # These are currently needed if you want to get graphite running locally
  DEV_VM_COOKBOOK_DIR = "#{ENV['OPSCODE_SRC']}/opscode-dev-vm/cookbooks"
  cookbook "piab",     path: "#{DEV_VM_COOKBOOK_DIR}/piab"
  cookbook "estatsd",  path: "#{DEV_VM_COOKBOOK_DIR}/estatsd"
  cookbook "graphite", path: "#{DEV_VM_COOKBOOK_DIR}/graphite"
  cookbook "gunicorn", path: "#{DEV_VM_COOKBOOK_DIR}/gunicorn"

  # This is my fork of the gdash cookbook from the community site
  # (needed to fix a small bug); we have a home-grown gdash in
  # preprod, but I've run into some issues using it.  We only really
  # want it here to test out the Bifrost dashboards, so it's not
  # necessary that it be exactly the same as in prod.
  cookbook "gdash", github: "christophermaier/chef-gdash", branch: "fix_config"

  # Use the community unicorn cookbook too for simplicity... again,
  # this is just for testing out dashboards
  cookbook "unicorn", site: :opscode
end
