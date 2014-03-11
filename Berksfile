chef_api :config

metadata

%w[ opscode-dev-shim opscode-users ].each do |cb|
  cookbook cb, :git => "git@github.com:opscode-cookbooks/#{cb}.git"
end

%w[ users sudo opscode-lb ].each do |cb|
  cookbook cb, :path => "~/oc/opscode-platform-cookbooks/cookbooks/#{cb}"
end

cookbook 'oc-id', :path => './files/cookbooks/oc-id'