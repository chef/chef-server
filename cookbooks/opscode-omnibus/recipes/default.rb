
bash "install python packages" do
  code <<BASH
pip install Sphinx==1.1.3
pip install Pygments==1.4
BASH
end
