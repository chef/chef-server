module ApplicationHelper
  delegate :sign_up_url, :to => "Settings"

  def chef_class_for(flash_type)
    classes = {
      success:  'success',
      error:    'alert',
      alert:    'warning',
      notice:   'info'
    }

    classes[flash_type.to_sym] || flash_type.to_s
  end
end
