# Be sure to include the organization name
# For example: chef_server_url = "https://chef.example.com/organizations/demo/"
chef_server_url = "https://${chef_server_url}/organizations/4thcoffee/"
#
# The client defined by client_name needs to be an admin user of the Chef Server org.
client_name = "janedoe"
client_key = "/tmp/janedoe.pem"

# Ohai data will be loaded from this file and used for the nodes' automatic attributes.

# chef-load will evenly distribute the number of nodes across the desired interval (minutes)
# Examples:
#   30 nodes / 30 minute interval =  1 chef-client run per minute
# 1800 nodes / 30 minute interval = 60 chef-client runs per minute
num_nodes = '${num_nodes}'
interval = '${interval}'

# During the same interval of time, it is also possible to load a number of Chef actions
num_actions = 30

# This prefix will go at the beginning of each node name.
# This enables running multiple instances of chef-load without affecting each others' nodes
# For example, a value of "chef-load" will result in nodes named "chef-load-1", "chef-load-2", ...
node_name_prefix = "chef-load"
