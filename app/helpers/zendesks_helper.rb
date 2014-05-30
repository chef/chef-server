require 'zendesk_sso_url'

module ZendesksHelper
  def zendesk_enabled?
    Settings.zendesk.shared_secret.present? &&
      Settings.zendesk.subdomain.present?
  end

  def zendesk_sso_url(user, return_to = nil)
    ZendeskSSOURL.new(user, return_to, Settings.zendesk).to_s
  end
end
