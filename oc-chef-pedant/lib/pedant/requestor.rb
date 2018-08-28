# Copyright: Copyright 2012-2018 Chef Software, Inc.
# License: Apache License, Version 2.0
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

require 'openssl'
require 'mixlib/authentication/signedheaderauth'
require 'erubis'
require 'pathname'
require 'tmpdir'

module Pedant
  # Encapsulate a user / client of the Chef REST API
  class Requestor
    attr_reader :name, :signing_key, :admin, :preexisting, :platform

    # preexisting defaults to true to protect against inadvertent deletion
    def initialize(name, key, options = {})
      options[:preexisting] = true if options[:preexisting].nil?

      @name = name
      @platform = options[:platform]
      @bogus = options[:bogus]

      @signing_key = case key
                    when String then
                      raw = if key =~ /^-----BEGIN (RSA )?PRIVATE KEY-----.*/
                              key
                            else
                              IO.read(key).strip
                            end
                      OpenSSL::PKey::RSA.new(raw)
                    when OpenSSL::PKey::RSA then key
                    else fail "Unknown key type. Must be String or OpenSSL::PKey::RSA. #{key.inspect}"
                    end
      @preexisting = options[:preexisting]
      @admin = !!options[:admin]
    end

    def bogus?; @bogus; end

    # Generate authentication headers for a request from this Requestor.
    # Note that 'url' can either be a complete URL or just the path to the resource.
    #
    # `timestamp` is a parameter in order to verify that the platform behaves appropriately
    # when a signed request has expired.
    def signing_headers(method, url, body)
      signing_object = Mixlib::Authentication::SignedHeaderAuth.signing_object(:http_method => method,
                                                                               :path => URI.parse(url).path,
                                                                               :body => body,
                                                                               :timestamp => Time.now.utc.iso8601,
                                                                               :user_id => @name)
      signing_object.sign(@signing_key)
    end

    def knife_dir
      @_knife_dir ||= Dir.mktmpdir('dot-chef-', File.join(Dir.tmpdir, "oc-chef-pedant"))
    end

    def knife_rb_path
      "#{knife_dir}/knife.rb"
    end

    # Generate a knife.rb file from a template for a given user.
    # Prefer calling +populate_dot_chef+ over calling this directly.
    def generate_knife_files!
      # The template file is currently located right next to this
      # source file... seemed like the sanest place for it at the time
      template_data = File.read(Pathname.new(__FILE__).dirname.join("knife.rb.erb"))
      template = Erubis::Eruby.new(template_data)
      File.open(knife_rb_path, 'w') do |f|
        f.write(template.result(org_name:            platform.org_name,
                                knife_user:          name,
                                key_dir:             knife_dir,
                                server_url:          platform.api_url,
                                test_repository_dir: platform.test_repository_path))
      end
    end

    # Write out the key out as a PEM file. The name of the PEM file will be "USER_NAME.pem".
    def generate_user_pem!
      File.open("#{knife_dir}/#{name}.pem", 'w') {|f| f.write(signing_key)}
    end

    # +server_url+ becomes +chef_server_url+ in the generated knife.rb
    # file.  +key_dir+ is where the client_key and validator_key are
    # generated.  +knife_destination+ is complete path to the knife.rb
    # file, including the filename (e.g. /foo/bar/knife.rb).  This
    # allows for us to create multiple knife.rb files for different
    # users / testing scenarios.
    def populate_dot_chef!
      generate_knife_files!
      generate_user_pem!
    end

    def delete!
      fail "Define #delete!"
    end

  end

  class User < Requestor
    attr_reader :associate

    def initialize(name, key, options = {})
      # For platforms with multitenancy
      @associate = !!options[:associate]
      super(name, key, options)
    end

    def delete!
      platform.delete_user(self)
    end
  end

  class Client < Requestor
    attr_reader :validator
    def initialize(name, key, options = {})
      super(name, key, options)
      @validator = !!options[:validator]
    end

    def delete!
      platform.delete_client(self)
    end
  end

end
