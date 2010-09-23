require 'opscode/expander/configuration'

module Opscode
  module Expander
    # Flattens and expands nested Hashes representing Chef objects (e.g, Nodes,
    # Roles, DataBagItems, etc.) into flat Hashes with "expando" fields so
    # the objects are suitable to be saved into Solr. This code is more or less
    # copy-pasted from chef/solr/index which may or may not be a great idea,
    # though that does minimize the dependencies and hopefully minimize the
    # memory use of opscode-expander.
    class Flattener

      # including this module into Flattener will enable 'expando' fields.
      module ExpandoFieldsOn
        def add_field_value(keys, value)
          value = value.to_s
          each_expando_field(keys) { |expando_field| @flattened_item[expando_field] << value }
          @flattened_item[keys.join(UNDERSCORE)] << value
          @flattened_item[keys.last] << value
        end

        def each_expando_field(keys)
          return if keys.size == 1
          0.upto(keys.size - 1) do |index|
            original = keys[index]
            keys[index] = X
            yield keys.join(UNDERSCORE)
            keys[index] = original
          end
        end
      end

      # Including this module (default setting) disables the expando fields
      # feature. This can be overridden by including ExpandoFieldsOn
      module ExpandoFieldsOff
        def add_field_value(keys, value)
          value = value.to_s
          @flattened_item[keys.join(UNDERSCORE)] << value
          @flattened_item[keys.last] << value
        end
      end

      include ExpandoFieldsOff

      def self.enable_expando_fields
        include ExpandoFieldsOn
      end

      UNDERSCORE              = '_'
      X                       = 'X'

      X_CHEF_id_CHEF_X        = 'X_CHEF_id_CHEF_X'
      X_CHEF_database_CHEF_X  = 'X_CHEF_database_CHEF_X'
      X_CHEF_type_CHEF_X      = 'X_CHEF_type_CHEF_X'

      def initialize(item)
        @item = item
      end

      def flattened_item
        @flattened_item || flatten_and_expand
      end

      def flatten_and_expand
        @flattened_item = Hash.new {|hash, key| hash[key] = []}

        @item.each do |key, value|
          flatten_each([key.to_s], value)
        end

        @flattened_item.each_value { |values| values.uniq! }
        remove_blacklisted_keys
        @flattened_item
      end

      def remove_blacklisted_keys
        Expander.config.blacklisted_fieldnames.each do |blacklist_regex|
          @flattened_item.keys.each do |key|
            @flattened_item.delete(key) if key.match(blacklist_regex)
          end
        end
      end

      def flatten_each(keys, values)
        case values
        when Hash
          values.each do |child_key, child_value|
            add_field_value(keys, child_key)
            flatten_each(keys + [child_key.to_s], child_value)
          end
        when Array
          values.each { |child_value| flatten_each(keys, child_value) }
        else
          add_field_value(keys, values)
        end
      end

    end
  end
end