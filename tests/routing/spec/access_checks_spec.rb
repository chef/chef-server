require 'spec_helper.rb'
include SpecHelper

[:internal, :external, :intacct, :intchef].each do |lbtype|
  describe "#{lbtype} lb access checks" do
    before do
      @redis = redis_inst(lbtype)
    end
    describe "when global maint mode is set" do
      before do
        set_global_maint_mode(@redis)
      end
      after(:each) do
        clear_global_maint_mode(@redis)
        [MY_IP, MY_24, OTHER_IP, OTHER_24].each do |ip|
          dewhitelist(@redis, ip)
        end
      end
      it "503 if I'm not whitelisted" do
        make_request :type => lbtype
        code.should == "503"
      end
      it "503 if someone else is whitelisted" do
        whitelist(@redis, OTHER_IP)
        make_request :type => lbtype
        code.should == "503"
      end
      it "200 if I am whitelisted by IP" do
        whitelist(@redis, MY_IP)
        make_request :type => lbtype
        code.should == "200"
      end
      it "200 if I am whitelisted by subnet" do
        whitelist(@redis, MY_24)
        make_request :type => lbtype
        code.should == "200"
      end
    end
    describe "banned ips" do
      after(:each) do
        [MY_IP, MY_24, OTHER_IP, OTHER_24].each do |ip|
          unban(@redis, ip)
        end
      end

      it "403 when my ip is banned" do
        ban(@redis, MY_IP)
        make_request :type => lbtype
        code.should == "403"
      end

      it "403 when my subnet is banned" do
        ban(@redis, MY_24)
        make_request :type => lbtype
        code.should == "403"
      end

      it "403 when my ip and a different ip is banned" do
        ban(@redis, MY_IP)
        ban(@redis, OTHER_IP)
        make_request :type => lbtype
        code.should == "403"
      end

      it "200 when a different ip is banned" do
        ban(@redis, OTHER_IP)
        make_request :type => lbtype
        code.should == "200"
      end

      it "200 when a different subnet is banned" do
        ban(@redis, OTHER_24)
        make_request :type => lbtype
        code.should == "200"
      end
    end
  end
end
