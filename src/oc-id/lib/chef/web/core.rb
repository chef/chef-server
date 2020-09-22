require 'chef/web/core/engine' if Object.const_defined?(:Rails)
require 'chef/web/core/version'

class Chef
  module Web
    module Core
      ROOT_PATH = File.expand_path('../../..', File.dirname(__FILE__))
      TEMPLATES_PATH = File.join(ROOT_PATH, 'templates')
    end
  end
end