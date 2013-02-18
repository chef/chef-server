chef_api :config
site :opscode

cookbook "opscode-authz", git: "git@github.com:opscode-cookbooks/opscode-authz"

# These are only needed for local dev work
cookbook "opscode-dev-shim", git: "git@github.com:opscode-cookbooks/opscode-dev-shim.git",
                             branch: "2f27be5dd022bb0105318439add2ff83b79ca5b5" # don't use searchef just yet... breaks HTTP requests!
cookbook "chef-solo-search", git: "git://github.com/edelight/chef-solo-search"
