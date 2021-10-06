
class OpscodeWebuiDeprecationNotice
  attr_reader :opscode_webui_attribute_keys

  def initialize(opscode_webui_attributes)
    @opscode_webui_attribute_keys = (opscode_webui_attributes || {}).keys
  end

  def applicable?
    @applicable ||= !opscode_webui_attribute_keys.empty?
  end

  def message
    <<-EOS


    Your configuration file contains the following deprecated settings:

    #{formatted_attributes_for_message}

    These settings will have no effect and should be removed.
    EOS
  end

  private

  def formatted_attributes_for_message
    @formatted_attributes_for_message ||= opscode_webui_attribute_keys.map do |key|
      "  opscode_webui['#{key}']"
    end.join('\n')
  end
end
