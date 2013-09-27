#!/usr/bin/env ruby

require 'aws'
require 'erubis'
require 'uri'
require 'bitly'

module OCTechPreview

  class WikiPage
    def initialize(preview_info)
      @preview_info = preview_info
    end

    def text
      t = Erubis::Eruby.new(template)
      data = {:info => @preview_info.as_hash, :days_valid => @preview_info.days_valid}
      t.result(data)
    end

    def template
      File.read("confluence_page.erb")
    end
  end

  class URLGenerator
    attr_reader :days_valid

    def initialize(id, secret, bucket, days_valid, bitlyuser, bitlyapi)
      @id, @secret, @bucket, @days_valid, @bitlyuser, @bitlyapi = id, secret, bucket, days_valid, bitlyuser, bitlyapi
      @config = AWS.config(:access_key_id => @id, :secret_access_key => @secret, :region => 'us-west-2')
      @s3 = AWS::S3.new
      # Bitly library makes us declare that we're using api version 3 for now
      Bitly.configure do |config|
        config.api_version = 3
        config.login = @bitlyuser
        config.api_key = @bitlyapi
      end
      @bitly = Bitly.client
    end

    def url_for(package)
      o = @s3.buckets[@bucket].objects[package]
      url = o.url_for(:get, :expires => (60 * 60 * 24 * @days_valid))
      @bitly.shorten(url).short_url
    end

  end

  class PreviewInfo
    def initialize(packages, url_generator)
      @packages = packages
      @url_generator = url_generator
    end

    def days_valid
      @url_generator.days_valid
    end

    def as_hash
      @hash ||= begin
                  @packages.inject({}) do |hash, package|
                    info = PackageInfo.new(package)
                    url = @url_generator.url_for(package)
                    entry = {info.product => [[info.label, url]]}
                    hash.merge!(entry) do |key, oldval, newval|
                      oldval << newval.first
                    end
                    hash
                  end
                end
    end
  end

  class PackageInfo
    attr_reader :package

    def initialize(package)
      @package = package
    end

    def label
      l = platform
      l << " (#{architecture})" if architecture
      l
    end

    def platform
      case @package
      when /\.el5\./
        "CentOS 5"
      when /\.el6\./
        "Centos 6"
      when /ubuntu\.10\.04/
        "Ubuntu 10.04"
      when /ubuntu\.11\.04/
        "Ubuntu 11.04"
      when /ubuntu\.12\.04/
        "Ubuntu 12.04"
      when /windows\.msi/
        "Windows"
      when /\.gem/
        "All Platforms"
      else
        raise "Unrecognized package format for '#{@package}; could not determine platform!"
      end
    end

    # @returns [String, nil] nil if no architecture is detected (this
    # happens with e.g., Windows and Gem packages, and is not an
    # error)
    def architecture
      case @package
      when /x86_64/, /amd64/
        "64-bit"
      when /i686/, /i386/
        "32-bit"
      end
    end

    def product
      case @package
      when /private-chef/
        :private_chef
      when /opscode-reporting/
        :reporting
      when /knife-reporting/
        :knife_reporting
      when /opscode-push-jobs-server/
        :push_jobs_server
      when /opscode-push-jobs-client/
        :push_jobs_client
      when /knife-pushy/
        :knife_push_jobs
      when /opscode-webui/
        :webui
      else
        raise "Unrecognized package format for '#{@package}; could not determine product!"
      end
    end

  end
end

if __FILE__ == $0

  BUCKET_NAME = "opc11-tech-preview"
  DAYS_VALID  = 7

  id        = ENV['AWS_ACCESS_KEY_ID']     || raise("No AWS_ACCESS_KEY_ID environment variable set!")
  secret    = ENV['AWS_SECRET_ACCESS_KEY'] || raise("No AWS_SECRET_ACCESS_KEY environment variable set!")
  bitlyuser = ENV['BITLY_USER']            || raise("No BITLY_USER environment variable set!")
  bitlyapi  = ENV['BITLY_APIKEY']          || raise("No BITLY_APIKEY environment variable set!")


  packages = [
              # Private Chef
              "private-chef-11.0.0_rc.2-1.el5.x86_64.rpm",
              "private-chef-11.0.0_rc.2-1.el6.x86_64.rpm",
              "private-chef_11.0.0-rc.2-1.ubuntu.10.04_amd64.deb",
              "private-chef_11.0.0-rc.2-1.ubuntu.11.04_amd64.deb",
              "private-chef_11.0.0-rc.2-1.ubuntu.12.04_amd64.deb",

              # Reporting
              "opscode-reporting-0.2.2_tech.preview.2-1.el5.x86_64.rpm",
              "opscode-reporting-0.2.2_tech.preview.2-1.el6.x86_64.rpm",
              "opscode-reporting_0.2.2-tech.preview.2-1.ubuntu.10.04_amd64.deb",
              "opscode-reporting_0.2.2-tech.preview.2-1.ubuntu.11.04_amd64.deb",

              "knife-reporting-0.1.0.gem",

              # Pushy
              "opscode-push-jobs-server-1.0.0_rc.1-1.el5.x86_64.rpm",
              "opscode-push-jobs-server-1.0.0_rc.1-1.el6.x86_64.rpm",
              "opscode-push-jobs-server_1.0.0-rc.1-1.ubuntu.10.04_amd64.deb",
              "opscode-push-jobs-server_1.0.0-rc.1-1.ubuntu.11.04_amd64.deb",
              "opscode-push-jobs-server_1.0.0-rc.1-1.ubuntu.12.04_amd64.deb",

              "opscode-push-jobs-client-0.0.1+20130307153525.git.98.c04f587-1.windows.msi",
              "opscode-push-jobs-client-1.0.0_rc.1+20130918224717-1.el5.i686.rpm",
              "opscode-push-jobs-client-1.0.0_rc.1+20130918224717-1.el5.x86_64.rpm",
              "opscode-push-jobs-client-1.0.0_rc.1+20130918224717-1.el6.i686.rpm",
              "opscode-push-jobs-client-1.0.0_rc.1+20130918224717-1.el6.x86_64.rpm",
              "opscode-push-jobs-client_1.0.0-rc.1+20130918224717-1.ubuntu.10.04_i386.deb",
              "opscode-push-jobs-client_1.0.0-rc.1+20130918224717-1.ubuntu.10.04_amd64.deb",
              "opscode-push-jobs-client_1.0.0-rc.1+20130918224717-1.ubuntu.11.04_i386.deb",
              "opscode-push-jobs-client_1.0.0-rc.1+20130918224717-1.ubuntu.11.04_amd64.deb",
              "opscode-push-jobs-client_1.0.0-rc.1+20130918224717-1.ubuntu.12.04_i386.deb",
              "opscode-push-jobs-client_1.0.0-rc.1+20130918224717-1.ubuntu.12.04_amd64.deb",

              "knife-pushy-0.3.gem",

              # Web UI
              "opscode-webui-2.4.0_tech.preview.1-1.el5.x86_64.rpm",
              "opscode-webui-2.4.0_tech.preview.1-1.el6.x86_64.rpm",
              "opscode-webui_2.4.0-tech.preview.1-1.ubuntu.10.04_amd64.deb",
              "opscode-webui_2.4.0-tech.preview.1-1.ubuntu.11.04_amd64.deb"
             ]

  url_generator = OCTechPreview::URLGenerator.new(id, secret, BUCKET_NAME, DAYS_VALID, bitlyuser, bitlyapi)
  preview_info  = OCTechPreview::PreviewInfo.new(packages, url_generator)
  wiki_page     = OCTechPreview::WikiPage.new(preview_info)

  puts wiki_page.text
end
