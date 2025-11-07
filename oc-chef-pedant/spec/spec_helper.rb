# spec_helper.rb for oc-chef-pedant
# This allows running individual spec files with `bundle exec rspec spec/some_spec.rb`
# without needing the full bin/oc-chef-pedant setup

require "bundler/setup"
require "rspec"
require "pedant"

# Minimal Pedant configuration for local testing
# Note: Most tests will fail without a running Chef Server, but this allows
# verifying that the code loads without ActiveSupport errors

module Pedant
  module TestConfig
    def self.setup_minimal_config
      # Set minimal required config to avoid crashes during load
      Pedant.config.suite = %w{api}
      
      # Disable logging to avoid file system errors on Windows
      Pedant.config.log_file = nil
      
      # Create mock platform to avoid needing a real Chef Server
      mock_platform = Object.new
      def mock_platform.before_configure_rspec; end
      def mock_platform.configure_rspec; end
      def mock_platform.setup; end
      def mock_platform.cleanup; end
      def mock_platform.pedant_run_timestamp; Time.now.to_i; end
      def mock_platform.stats_password; 'mock_password'; end
      def mock_platform.webui_key; nil; end  # OSS specs check for this
      def mock_platform.api_url(path); "http://localhost#{path}"; end
      
      # Return a valid PEM key for mock requestors
      def mock_platform.bogus_key
        "-----BEGIN RSA PRIVATE KEY-----
MIIEogIBAAKCAQEA7TfhToresZudM5gaBfzM/eHrGuJtN8uMaG51fLk9rNVwOxIw
eb9AmWJSQgftRj4AJSnt8Jv+QyAafjg79rEmCpd2K+toN1fXiVsf/ld/VdMI5vCd
usJc8aC4OFIVrLetm9eq+joXiCSLfEYP1w4d1gc9wqr54rnGVgQdWv31NhqXX7Tl
2bHRoEsfIFM7Vr5zC9Rv6XEtrwezSpzQ+ru707UOCOHNnH1TDxNDz+L71xJnGXD6
uh596hVm2GiXJ8lyCVJTs/RZ+HKtRuxYOJztdCQAXbd6DDjTBax/cCEzzoxK/LMc
1K/zWCKN8yM/XpiFqNuBB0yrqS5Y4zlROY0ssQIDAQABAoIBAQDmC1HYvD1YGePq
O+/IrK8S6jr4WGq4OBIS2EPhTzbrXBU5g9s0xe7ckIfa9xr4CnpTkATqWCzMZd6r
Vtd31bVhgh6cWu829F3WG2O8YJfg4AX7B46+pWxC+qyMGbZhR8L5pb1ualWVtnL6
cms8D7mJbH5NQUeRwrz/f4AEVNGuw165PhglXnRq3zxs97jdNH6av0na1VLAmUeR
vmYToLoIsK5yLeNzMfRRrQkq4ndUvad2ahqsz+p/xUjfYneBz4uujE/PcCgi423Q
FMH1NtPpzmuZuABU53wUMW4BXsSGv+34UMhRTazcBPRBYjKT6l6F7Tt9wPFxrtVn
KYI9dI0tAoGBAPzBwLsUuyEJtSd2zW63f7EhhG/2rSBm1g9Bcs1sHZtXIX3JN51X
hd+ckXQcLgQYn8syGW1+WcuaHGyp7v3G0yr6b38FWwEnmD85Mq5da1dyio2udBdK
Xm8MfY94yP8qSH3bUDKEl+cV9X5rtzbQAorMXb/Qkc4vErWfABVBKeLPAoGBAPBD
FjGxER9YU4hFV2vn5qElfxa799Vl0uSvao0lSkTpA/sHFxAkRQc/mo1PBClaH0ba
Hqr141o5pGUDgLqpO3kEY5vYBOaFXLFdFCcL+1YUR6t0bX+WCHuq21Cs6Gu4+qNA
D4dpsSPDXfatyXWM5PF2d4FwO2XnL25Yt+rg6dh/AoGAbEfk9UCQWjr6bImswH3E
KnIddonK6VKk6aw0LmTe2imdo3GMbc+M/prohUF9RSv3aOlxk0LJ3TuMadDzHa0L
0iGvmk8FCZ2Yz50FZUWIMtJTIRdXjJLDmfdT4x7vnMDUhXZrCPlcyhbSMPKcbtL2
A9hBYWdMz3PDJCOVuYVNGGkCgYEAhSxKUwTYfs1Qj8oPqOoDdfL4vLs3pfsoqEVr
BA1VW1jlMfE+IV5ZPKlOm2De56TijT09nnloqYwlyS/l3JENPAjoxWs5XCUzucPj
9bi4eYAIMcr5Hq0kypdrtQ4CTiNcGbzaXq6A11fk72RotFWCWSzXFNIGuncoXTuj
xfcg5zUCf3TQrwpcBB1Hf9hm6lUVpptvUZJAQtkWoaWXUPRY0CjcVdDc5ak4xqL2
FcAuJwV95b3qLyPESJ8zUBLOg3DcT1X9mrvRuKgC/Ie6k4R+oxU/nXi4vsPXlvg4
yap6MUYSjPOa7eCrhg2zFZiqO6VLEogPc1nsjb9Zl2UWLLYyCVz=
-----END RSA PRIVATE KEY-----"
      end
      
      def mock_platform.admin_user; Pedant::Requestor.new("admin", bogus_key, admin: true, preexisting: true); end
      def mock_platform.non_admin_user; Pedant::Requestor.new("normal", bogus_key, preexisting: true); end
      
      # Set the platform before configure_rspec is called
      Pedant.config.define_singleton_method(:pedant_platform) { mock_platform }
      
      # Configure RSpec with Pedant's settings
      Pedant.configure_rspec
    end
  end
end

# Initialize Pedant for testing
Pedant::TestConfig.setup_minimal_config

puts "Pedant test environment initialized (minimal mode)"
puts "Note: Tests requiring a running Chef Server will fail, but you can verify code loads correctly"
