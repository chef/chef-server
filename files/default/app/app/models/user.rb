class User 
  extend ActiveModel::Naming
  include ActiveModel::AttributeMethods
  include ChefResource

  attr_accessor :username, :password, :first_name, :last_name, :email, :public_key

  def initialize(attrs={})
    attrs.each do |k,v|
      instance_variable_set("@#{k}", v) unless v.nil?
    end
  end

  def id
    username
  end

  def to_param
    username
  end

  def public
    {
      username: username,
      first_name: first_name,
      last_name: last_name
    }
  end

  def url
    "users/#{username}"
  end

  def assign_attributes(resource)
    @first_name = resource['first_name']
    @last_name = resource['last_name']
    @public_key = resource['public_key']
    @email = resource['email']
  end

  def update_attributes(attrs)
    assign_attributes(attrs) # And then save
  end

  class << self
    def find(username)
      begin
        new(username: username).get unless username.nil?
      rescue Net::HTTPServerException

      end
    end
  end
end
