
link "/opt/opscode/embedded/bin/dvm" do
  to "/vagrant/dvm/bin/dvm"
end


# Load the components that have been requested
node["omnibus-autoload"].each do |component|
  ruby_block "dvm load omnibus #{component}" do
    block do
       require "/vagrant/dvm/lib/dvm"
       DVM::Application.start(["load", "omnibus", component, "--no-build"])
    end
  end
end
