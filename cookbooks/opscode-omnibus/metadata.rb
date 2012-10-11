maintainer        "Opscode, Inc."
maintainer_email  "cookbooks@opscode.com"
license           "Apache 2.0"
description       "Opscode Omnibus base O/S configuration"
long_description  IO.read(File.join(File.dirname(__FILE__), 'README.md'))
version           "0.10.0"

%w{ ubuntu centos }.each do |os|
  supports os
end

%w{ python }.each do |cb|
  depends cb
end
