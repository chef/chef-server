require 'mixlib/authentication/signatureverification'

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

    def authenticate(username, password)
      begin
        self.find(username) if self.new.chef.post_rest('authenticate_user', { username: username, password: password })
      rescue Net::HTTPServerException

      end
    end

    # Take a request object and verify the user is valid based on the signed
    # request and the X-Ops-UserId header.
    #
    # Returns the user if it is valid, or nil if not.
    def from_signed_request(request)
      user = User.find(request.headers['x-ops-userid'])
      if user
        #verifier = Mixlib::Authentication::SignatureVerification.new(request)
        #public_key = OpenSSL::PKey::RSA.new user.public_key
        #if verifier.authenticate_request(public_key)
          #user
        #else
          #nil
        #end
      else
        nil
      end
    end
  end
end
