require 'json'
require 'pedant/rspec/common'
require 'chef-utils/dist'

describe "running configs required by Reporting", :config do
  let (:config) { JSON.parse(IO.read("/etc/#{::ChefUtils::Dist::Org::LEGACY_CONF_DIR}/#{::ChefUtils::Dist::Server::SERVER}-running.json"))['private_chef'] }

  context "oc-reporting-pedant" do

    it "api_version" do
      expect(config['api_version'].to_s).not_to eq ''
    end

    it "nginx/ssl_protocols" do
      expect(config['nginx']['ssl_protocols'].to_s).to_not eq ''
    end
  end

  context "nginx" do
    it "nginx/dir" do
      expect(File.exists?(File.join(config['nginx']['dir'], "etc/addon.d"))).to be true
    end
  end

  context "providers/pg_upgrade" do

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
  end

  context "pedant-config" do

    it "lb/api_fqdn" do
      expect(config['lb']['api_fqdn'].to_s).to_not eq ''
    end

    it "opscode-erchef/search_provider" do
      expect(config['opscode-erchef']['search_provider'].to_s).to eq('solr').or eq('elasticsearch')
    end

    it "opscode-erchef/listen" do
      expect(config['opscode-erchef']['listen'].to_s).not_to eq ''
    end

    it "opscode-erchef/port" do
      expect(config['opscode-erchef']['port'].to_i).not_to eq 0
    end
  end

  context "oc-reporting-config" do

    it "oc_bifrost/vip" do
      expect(config['oc_bifrost']['vip'].to_s).not_to eq ''
    end

    it "oc_bifrost/port" do
      expect(config['oc_bifrost']['port'].to_i).not_to eq 0
    end

    it "couchdb/vip" do
      expect(config['couchdb']['vip'].to_s).not_to eq ''
    end

    it "couchdb/port" do
      expect(config['couchdb']['port'].to_i).not_to eq 0
    end

    it "nginx/enable_ipv6" do
      expect(config['nginx']['enable_ipv6']).to be(true).or be(false)
    end

    it "opscode-erchef/search_provider" do
      expect(config['opscode-erchef']['search_provider'].to_s).to eq('solr').or eq('elasticsearch')
    end

    it "opscode-erchef/solr_timeout" do
      expect(config['opscode-erchef']['solr_timeout'].to_i).not_to eq 0
    end

    it "opscode-erchef/solr_http_init_count" do
      expect(config['opscode-erchef']['solr_http_init_count'].to_i).not_to eq 0
    end

    it "opscode-erchef/solr_http_max_count" do
      expect(config['opscode-erchef']['solr_http_max_count'].to_i).not_to eq 0
    end

    it "opscode-erchef/solr_http_cull_interval" do
      expect(config['opscode-erchef']['solr_http_cull_interval'].to_s).not_to eq ''
    end

    it "opscode-erchef/solr_http_max_age" do
      expect(config['opscode-erchef']['solr_http_max_age'].to_s).not_to eq ''
    end

    it "opscode-erchef/solr_http_max_connection_duration" do
      expect(config['opscode-erchef']['solr_http_max_connection_duration'].to_s).not_to eq ''
    end

    it "opscode-erchef/solr_ibrowse_options" do
      expect(config['opscode-erchef']['solr_ibrowse_options'].to_s).not_to eq ''
    end

    it "lb/api_fqdn" do
      expect(config['lb']['api_fqdn'].to_s).not_to eq ''
    end

    it "lb/vip" do
      expect(config['lb']['vip'].to_s).not_to eq ''
    end

    it "lb_internal/chef_port" do
      expect(config['lb_internal']['chef_port'].to_i).not_to eq 0
    end

    it "estatsd/vip" do
      expect(config['estatsd']['vip'].to_s).not_to eq ''
    end

    it "estatsd/port" do
      expect(config['estatsd']['port'].to_i).not_to eq 0
    end
  end
end
