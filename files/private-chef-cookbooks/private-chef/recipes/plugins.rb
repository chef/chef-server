
# Disable the specified plugins
node['private_chef']['plugins']['disabled'].each do |plugin|
  include_recipe "#{plugin}::disable"
end

# Enable the specified plugins
node['private_chef']['plugins']['enabled'].each do |plugin|
  include_recipe "#{plugin}::enable"
end
