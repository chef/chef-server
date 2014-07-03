if not PrivateChef['ha']['provider'].nil?
  include_recipe "ha::#{PrivateChef['ha']['provider']}"
end
