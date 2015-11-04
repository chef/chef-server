require 'mixlib/authentication/signatureverification'

class User
  extend ActiveModel::Naming
  include ActiveModel::Conversion

  include ActiveModel::AttributeMethods
  include ChefResource

  ATTRIBUTES = [
    :username,
    :first_name,
    :last_name,
    :middle_name,
    :email,
    :public_key,
    :private_key,
    :display_name,
    :password
  ]

  attr_accessor *ATTRIBUTES
  attr_reader :errors

  def initialize(attrs={})
    set_attributes(attrs)
    @errors = ActiveModel::Errors.new(self)
  end

  def id
    username
  end

  def to_param
    username
  end

  def regen_private_key
    update_attributes('private_key' => true)
  end

  def set_attributes(attrs={})
    attrs.symbolize_keys!
    ATTRIBUTES.each { |a| instance_variable_set("@#{a}", attrs[a]) unless attrs[a].nil? }
  end

  def update_password(params)
    [:current_password, :new_password, :password_confirmation].each do |f|
      if params[f].blank?
        errors.add(f, I18n.t("errors.passwords.blank", :label => f.to_s.titleize))
      end
    end

    unless User.authenticate(username, params[:current_password])
      errors.add(:current_password, I18n.t("errors.passwords.current_password_is_wrong"))
    end

    if params[:new_password] != params[:password_confirmation]
      errors.add(:base, I18n.t("errors.passwords.dont_match"))
    end

    if errors.empty?
      begin
        update_attributes('password' => params[:new_password])
      rescue Net::HTTPServerException => e
        raise
      end
    end
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

    begin
      result = chef.put_rest(url, new_attrs)
      set_attributes(new_attrs.merge('private_key' => result['private_key']))
    rescue => e
      false
    end
  end

  # ActiveModel::Errors requires a minimal implementation of these 3 methods
  def read_attribute_for_validation(attr)
    send(attr)
  end

  def User.human_attribute_name(attr, options={})
    attr.to_s.titleize
  end

  def User.lookup_ancestors
    [self]
  end
  # ActiveModel::Errors finished

  class << self
    def admin?(username)
      Settings.doorkeeper.administrators.include?(username)
    end

    def find(username)
      begin
        if (username != nil && username.include?('@'))
          users = self.new.chef.get(
            "users?#{{ email: username }.to_query}"
          )
          if (users.length > 0)
            username = users.first[0]
          end
        end
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
