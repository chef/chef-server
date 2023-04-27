# frozen_string_literal: true

require 'mixlib/authentication/signatureverification'

class User
  extend ActiveModel::Naming
  include ActiveModel::Conversion

  include ActiveModel::AttributeMethods
  include ChefResource

  ATTRIBUTES = %i[
    username
    first_name
    last_name
    middle_name
    email
    public_key
    private_key
    display_name
    password
  ].freeze

  attr_accessor(*ATTRIBUTES)
  attr_reader :errors

  def initialize(attrs = {})
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

  def set_attributes(attrs = {})
    attrs.symbolize_keys!
    ATTRIBUTES.each { |a| instance_variable_set("@#{a}", attrs[a]) unless attrs[a].nil? }
  end

  def update_password(params)
    %i[current_password new_password password_confirmation].each do |f|
      errors.add(f, I18n.t('errors.passwords.blank', label: f.to_s.titleize)) if params[f].blank?
    end

    unless User.authenticate(username, params[:current_password])
      errors.add(:current_password, I18n.t('errors.passwords.current_password_is_wrong'))
    end

    errors.add(:base, I18n.t('errors.passwords.dont_match')) if params[:new_password] != params[:password_confirmation]

    return unless errors.empty?

    begin
      update_attributes('password' => params[:new_password])
    rescue Net::HTTPServerException => e
      raise
    end
  end

  def public
    {
      username:,
      first_name:,
      last_name:
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

  def update_attributes(attrs = {})
    current_attrs = ATTRIBUTES.each_with_object({}) do |key, res|
      val = instance_variable_get("@#{key}".to_sym)
      res[key.to_s] = val unless val.nil?
    end

    new_attrs = current_attrs.merge(attrs)

    begin
      result = chef.put_rest(url, new_attrs)
      set_attributes(new_attrs.merge('private_key' => result['private_key']))
    rescue StandardError => e
      false
    end
  end

  # ActiveModel::Errors requires a minimal implementation of these 3 methods
  def read_attribute_for_validation(attr)
    send(attr)
  end

  def self.human_attribute_name(attr, _options = {})
    attr.to_s.titleize
  end

  def self.lookup_ancestors
    [self]
  end
  # ActiveModel::Errors finished

  class << self
    def admin?(username)
      Settings.doorkeeper.administrators.include?(username)
    end

    def find(username)
      if !username.nil? && username.include?('@')
        users = new.chef.get(
          "users?#{{ email: username }.to_query}"
        )
        username = users.first[0] if users.length.positive?
      end
      new(username:).get unless username.nil?
    rescue Net::HTTPServerException
    end

    def authenticate(username, password)
      find(username) if new.chef.post_rest('authenticate_user', { username:, password: })
    rescue Net::HTTPServerException
    end

    # Take a request object and verify the user is valid based on the signed
    # request and the X-Ops-UserId header.
    #
    # Returns the user if it is valid, or nil if not.
    def from_signed_request(request)
      user = User.find(request.headers['x-ops-userid'])
      return unless user

      verifier = Mixlib::Authentication::SignatureVerification.new(request)
      public_key = OpenSSL::PKey::RSA.new user.public_key
      return unless verifier.authenticate_request(public_key)

      user
    end
  end
end
