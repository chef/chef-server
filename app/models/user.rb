require 'mixlib/authentication/signatureverification'

class User
  extend ActiveModel::Naming

  include ActiveModel::AttributeMethods
  include ChefResource

  ATTRIBUTES = [
    :username,
    :first_name,
    :last_name,
    :email,
    :public_key,
    :display_name,
    :password
  ]

  attr_accessor *ATTRIBUTES

  def initialize(attrs={})
    set_attributes(attrs)
  end

  def id
    username
  end

  def to_param
    username
  end

  def set_attributes(attrs={})
    attrs.symbolize_keys!
    ATTRIBUTES.each { |a| instance_variable_set("@#{a}", attrs[a]) unless attrs[a].nil? }
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

  def organizations
    chef.get("users/#{username}/organizations").map do |organizations|
      organizations['organization']
    end
  rescue Net::HTTPServerException

  end

  def update_attributes(attrs={})
    current_attrs = ATTRIBUTES.reduce({}) do |res, key|
      val = instance_variable_get("@#{key}".to_sym)
      res[key.to_s] = val unless val.nil?
      res
    end

    new_attrs = current_attrs.merge(attrs)
    chef.put_rest(url, new_attrs)
    set_attributes(new_attrs)
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
        verifier = Mixlib::Authentication::SignatureVerification.new(request)
        public_key = OpenSSL::PKey::RSA.new user.public_key
        if verifier.authenticate_request(public_key)
          user
        else
          nil
        end
      else
        nil
      end
    end
  end
end
