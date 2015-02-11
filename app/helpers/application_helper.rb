module ApplicationHelper
  delegate :sign_up_url, :to => "Settings"

  def bootstrap_class_for(flash_type)

    classes = {
      success:  'alert-success',
      error:    'alert-danger',
      alert:    'alert-warning',
      notice:   'alert-info'
    }

    classes[flash_type.to_sym] || flash_type.to_s
  end

  def chef_domain
    ENV['CHEF_DOMAIN'] || 'chef.io'
  end

  def new_password_reset_url
    ENV['PASSWORD_RESET_URL'] || "https://#{chef_domain}/account/password_resets/new"
  end
end
