if not PrivateChef['ha']['provider'].nil?
  include_attribute "ha::#{PrivateChef['ha']['provider']}"
end
