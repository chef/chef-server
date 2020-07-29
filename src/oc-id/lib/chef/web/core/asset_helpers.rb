require 'chef/web/core'
require 'chef/web/core/component'
require 'nokogiri'

class Chef
  module Web
    module Core
      module AssetHelpers

        def chef_logo(element=:div, opts={})
          opts[:attributes] = opts.reject { |key| key == :data }
          opts[:data] ||= {}

          logo_classname = 'logo'
          classnames = (opts[:attributes][:class] || '').split(' ') 
          opts[:attributes][:class] = classnames.reject { |c| c == logo_classname }.push(logo_classname).join(' ')

          opts[:svg] = Nokogiri::XML(open(File.join(File.dirname(__FILE__), '../../../assets/images/chef-logo.svg')))

          if opts[:data][:'tag-line']
            opts[:svg].css('#chef-logo-tag-line text').first.content = opts[:data][:'tag-line']
          end

          attrs = opts[:attributes].map { |k, v| %Q(#{k}="#{v}") }
          attrs << opts[:data].map { |k, v| %Q(data-#{k}="#{v}") }
          opts[:attributes] = attrs.join(' ')

          Chef::Web::Core::Component.new(opts.merge!(:element => element, :type => 'logo')).render
        end

      end
    end
  end
end