include_recipe "nodejs"
include_recipe "omnibus"
include_recipe "python"

# We should consider making this proper config/software deps for
# private-chef-administration
python_pip "Sphinx" do
  version "1.1.3"
  action :install
end

python_pip "Pygments" do
  version "1.4"
  action :install
end
