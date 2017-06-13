require 'json'
require 'pedant/rspec/common'

describe "running configs required by chef-server-ctl", :config do
  let (:complete_config) { JSON.parse(IO.read("/etc/opscode/chef-server-running.json")) }
  let (:config) { complete_config['private_chef'] }

  it "fips_enabled" do
    expect(config['fips_enabled']).to be(true).or be(false)
  end

  it "opscode-erchef/search_queue_mode" do
    expect(config["opscode-erchef"]["search_queue_mode"]).to eq("rabbitmq")
                                                               .or eq("batch")
                                                                     .or eq("inline")
  end

  it "ldap/enabled" do
    expect(config["ldap"]["enabled"]).to be(true).or be(false).or be(nil)
  end

  it "runit/sv_dir" do
    expect(complete_config["runit"]["sv_dir"].to_s).to_not eq("")
    expect(File.exist?(complete_config["runit"]["sv_dir"])).to be(true)
  end

  context "redis_lb" do
    it "vip" do
      expect(config["redis_lb"]["vip"].to_s).to_not eq("")
    end

    it "port" do
      expect(config["redis_lb"]["vip"].to_i).to_not eq(0)
    end
  end

  context "keepalived" do
    it "enable" do
      expect(config['keepalived']['enable']).to be(true).or be(false)
    end

    it "vrrp_instance_ipaddress" do
      expect(config['keepalived']['vrrp_instance_ipaddress'])
    end

    it "vrrp_instance_ipaddress_dev" do
      expect(config['keepalived']['vrrp_instance_ipaddress_dev'])
    end

    it "vrrp_instance_interface" do
      expect(config['keepalived']['vrrp_instance_interface'])
    end
  end

end
