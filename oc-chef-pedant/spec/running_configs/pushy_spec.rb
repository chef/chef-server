require 'json'
require 'pedant/rspec/common'

describe "running configs required by Pushy Server", :config do
  let (:complete_config) { JSON.parse(IO.read("/etc/opscode/#{Chef::Dist::Server::SHORT}-running.json")) }
  let (:config) { complete_config['private_chef'] }

  it "role" do
    expect(config['role'].to_s).to eq("standalone").or eq("backend").or eq("frontend")
  end

  it "topology" do
    expect(config['topology'].to_s).to eq("tier").or eq("ha").or eq("standalone")
  end

  it "postgresql/vip" do
    expect(config['postgresql']['vip'].to_s).to_not eq ''
  end

  it "postgresql/port" do
    expect(config['postgresql']['port'].to_i).to_not eq 0
  end

  it "postgresql/db_superuser" do
    expect(config['postgresql']['db_superuser'].to_s).to_not eq ''
  end

  it "postgresql/vip or backend_vips/ipaddress" do
    if ["ha", "tier"].include? config['topology']
      expect(config['backend_vips']['ipaddress'].to_s).not_to eq ''
    else
      skip "not used for standalone"
    end
  end

  it "postgresql/data_dir" do
    if config['postgresql']['external']
      skip "not used for external postgresql"
    # These tests should not run on the frontend of a tier setup.
    elsif (Pedant::Config.topology == "tier" && Pedant::Config.role == "frontend")
      skip "no postgresql installed on frontend of a tier install"
    else
      expect(File.exists?(config['postgresql']['data_dir'])).to be true
    end
  end

  it "postgresql/username" do
    expect(config['postgresql']['username'].to_s).to_not eq ''
  end

  it "user/username" do
    expect(config['user']['username'].to_s).to_not eq ''
  end

  it "nginx/dir" do
    expect(File.exists?(File.join(config['nginx']['dir'], "etc/addon.d"))).to be true
  end

  it "nginx/ssl_protocols" do
    expect(config['nginx']['ssl_protocols'].to_s).to_not eq ''
  end

  it "opscode-erchef/strict_search_result_acls" do
    expect(config['opscode-erchef']['strict_search_result_acls']).to be(true).or be(false)
  end

  it "oc-chef-pedant/debug_org_creation" do
    expect(config['oc-chef-pedant']['debug_org_creation']).to be(true).or be(false)
  end

  it "runit" do
    expect(complete_config['runit'].class).to eq Hash
  end
end
