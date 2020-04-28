require 'json'
require 'pedant/rspec/common'

describe "running configs required by Manages", :config do
  let (:config) { JSON.parse(IO.read("/etc/opscode/#{Chef::Dist::Server::SHORT}-running.json"))['private_chef'] }

  it "lb/api_fqdn" do
    expect(config['lb']['api_fqdn'].to_s).to_not eq ''
  end

  it "nginx/ssl_port" do
    expect(config['nginx']['ssl_port'].to_i).to_not eq 0
  end

  it "nginx/ssl_protocols" do
    expect(config['nginx']['ssl_protocols'].to_s).to_not eq ''
  end

  it "nginx/ssl_ciphers" do
    expect(config['nginx']['ssl_ciphers'].to_s).to_not eq ''
  end

  it "nginx/ssl_certificate" do
    expect(File.exists?(config['nginx']['ssl_certificate'])).to be true
  end

  it "nginx/ssl_certificate_key" do
    expect(File.exists?(config['nginx']['ssl_certificate_key'])).to be true
  end

  it "nginx/dir" do
    expect(File.exists?(File.join(config['nginx']['dir'], "etc/addon.d"))).to be true
  end

  it "opscode-erchef/strict_search_result_acls" do
    expect(config['opscode-erchef']['strict_search_result_acls']).to be(true).or be(false)
  end

  it "ldap is a hash" do
    expect(config['ldap'].class).to eq(Hash)
  end
end
