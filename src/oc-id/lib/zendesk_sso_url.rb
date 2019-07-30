require 'cgi'
require 'jwt'
require 'securerandom'
require 'uri'

class ZendeskSSOURL
  attr_reader :user, :return_to, :settings

  def initialize(user, return_to, settings)
    @user = user
    @return_to = return_to
    @settings = settings
  end

  def to_s
    URI::HTTPS.build(
      host: host,
      path: path,
      query: query_hash.to_query,
    ).to_s
  end

  private

  def host
    "#{settings.subdomain}.zendesk.com"
  end

  def path
    '/access/jwt'
  end

  # From https://github.com/zendesk/zendesk_jwt_sso_examples/blob/master/ruby_on_rails_jwt.rb
  def payload
    iat = Time.now.to_i
    jti = "#{iat}/#{SecureRandom.hex(18)}"

    JWT.encode({
      iat: iat,
      jti: jti,
      name: [user.first_name, user.last_name].compact.join(' '),
      email: user.email,
    }, settings.shared_secret)
  end

  def query_hash
    q = { jwt: payload }
    q[:return_to] = CGI.escape(return_to) if return_to.present?
    q
  end
end
