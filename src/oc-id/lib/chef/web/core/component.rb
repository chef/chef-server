require 'erb'
require 'chef/web/core/url_helpers'

class Chef
  module Web
    module Core
      class Component
        include Chef::Web::Core::URLHelpers

        attr_accessor :type

        def initialize(opts={})
          opts.each do |k, v|
            instance_variable_set("@#{k}", v) unless v.nil?
          end
        end

        def render
          erb = ERB.new(open(File.join(Chef::Web::Core::TEMPLATES_PATH, "#{type}.html.erb")).read)
          erb.result(binding)
        end
      end
    end
  end
end