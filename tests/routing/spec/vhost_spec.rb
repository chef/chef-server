# Additional vhost tests that don't specifically relate to the chef API routing
# are housed here.
#

require 'spec_helper.rb'
include SpecHelper


describe "requests to opscode-certificate" do
  it "arrive at their destination" do
    location = "/anything/is/valid"
    make_request :type => :intcert, :location => location
    body.should == "certificate: #{strip_location(location)}"
  end
end
