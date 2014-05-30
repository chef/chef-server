module ZendesksHelper
  def zendesk_enabled?
    Settings.zendesk.shared_secret.present? &&
      Settings.zendesk.subdomain.present?
  end
end
