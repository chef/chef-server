
# Class for creating oc-id applications and getting information about them
class OCIDApplication
  include Chef::Mixin::ShellOut

  attr_reader :attributes
  attr_reader :name

  def initialize(name, attributes)
    @name = name
    @attributes = attributes.dup
  end

  def attributes
    @attributes.delete_if do |key|
      %w[ id name created_at updated_at ].include?(key)
    end
  end

  def create
    command = shell_out!(
      "bin/rails runner -e production '\
       app = Doorkeeper::Application.find_or_create_by(:name => \"#{name}\");\
       app.update_attributes(:redirect_uri => \"#{attributes['redirect_uri']}\");\
       puts app.to_json'",
      :cwd => '/opt/opscode/embedded/service/oc_id'
    )
    @attributes = Chef::JSONCompat.from_json(command.stdout)
    self
  end
end
