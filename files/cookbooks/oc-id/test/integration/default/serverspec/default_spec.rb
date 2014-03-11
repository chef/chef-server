require 'spec_helper'

describe 'The web app' do

  describe command('which ruby') do
    its(:stdout) { should match /ruby/ }
  end

  describe command('which git') do
    its(:stdout) { should match /git/ }
  end

  describe user('opscode') do
    it { should exist }
  end

  describe group('opscode') do
    it { should exist }
  end

  describe file('/etc/opscode/webui_priv.pem') do
    it { should be_file }
  end

  describe file('/srv/oc-id') do
    it { should be_directory }
  end

  # describe service('oc-id') do
  #   it { should be_enabled }
  # end

  # describe port(3000) do
  #   it { should be_listening }
  # end

end
