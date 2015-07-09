require 'chef/web/core/asset_helpers'
require 'chef/web/core/url_helpers'

module ApplicationHelper
  include Chef::Web::Core::AssetHelpers
  include Chef::Web::Core::URLHelpers

  def error_message_for(model, attr)
    msgs = model.errors[attr]

    if msgs.present?
      content_tag(:small, msgs.join("\n"), :class => 'error')
    end
  end

  def chef_class_for(flash_type)
    classes = {
      success:  'success',
      error:    'alert',
      alert:    'warning',
      notice:   'info'
    }

    classes[flash_type.to_sym] || flash_type.to_s
  end

  def delete_button(text, url)
    button_to(
      text,
      url,
      :class => 'button alert',
      :method => :delete,
      :data => {
        'abide' => '',
        'confirm' => 'Are you sure?'
      }
    )
  end
end
