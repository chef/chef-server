
def load_settings
  attributes = YAML.load_file("defaults.yml")
  begin
    custom_attributes = YAML.load_file("config.yml")
    attributes = simple_deep_merge(attributes, custom_attributes)
  rescue
  end
  attributes
end
def simple_deep_merge(source_hash, new_hash)
  source_hash.merge(new_hash) do |key, old, new|
    if new.respond_to?(:blank) && new.blank?
      old
    elsif (old.kind_of?(Hash) and new.kind_of?(Hash))
        simple_deep_merge(old, new)
    elsif (old.kind_of?(Array) and new.kind_of?(Array))
        old.concat(new).uniq
    else
       new
    end
  end
end
