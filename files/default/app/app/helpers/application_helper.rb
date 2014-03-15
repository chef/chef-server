module ApplicationHelper

  def bootstrap_class_for(flash_type)

    classes = {
      success:  'alert-success',
      error:    'alert-danger',
      alert:    'alert-warning',
      notice:   'alert-info'
    }

    classes[flash_type] || flash_type.to_s
  end
  
end
