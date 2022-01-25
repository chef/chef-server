
link "/opt/opscode/embedded/bin/dvm" do
  to "/vagrant/dvm/bin/dvm"
end


# Autoload omnibus components (such as cookbooks) that should be in place
# before the first `chef-server-ctl reconfigure` is run.
node["omnibus-autoload"].each do |component|
  ruby_block "dvm load omnibus #{component}" do
    block do
       require "/vagrant/dvm/lib/dvm"
       DVM::Application.start(["load", "omnibus", component, "--no-build"])
    end
  end
end
