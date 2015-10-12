
link "/opt/opscode/embedded/bin/dvm" do
  to "/vagrant/dvm/bin/dvm"
end


# Load the components that have been requested
node["omnibus-autoload"].each do |component|
  execute "dvm load omnibus #{component}"  do
    # Use -no-build to avoid triggering multiple possible automatic reconfigures
    # within DVM - we'll do it ourselves in the next step instead.
    command "dvm load omnibus #{component} --no-build"
  end
end


