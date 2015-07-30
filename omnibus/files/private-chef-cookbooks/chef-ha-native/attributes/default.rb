#######
# Bookshelf Receiver
#######
default['private_chef']['bookshelf-receiver']['enable'] = true
default['private_chef']['bookshelf-sender']['ha'] = true
default['private_chef']['bookshelf-receiver']['dir'] = "/var/opt/opscode/bookshelf-receiver"
default['private_chef']['bookshelf-receiver']['log_directory'] = "/var/log/opscode/bookshelf-receiver"
default['private_chef']['bookshelf-receiver']['log_rotation']['file_maxbytes'] = 104857600
default['private_chef']['bookshelf-receiver']['log_rotation']['num_to_keep'] = 10
default['private_chef']['bookshelf-receiver']['listen'] = '0.0.0.0'
default['private_chef']['bookshelf-receiver']['port'] = 11873
