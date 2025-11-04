# Simple Hash#stringify_keys implementation replacing ActiveSupport usage.
# Adds the method only if it's not already defined to avoid clobbering.
# Used by various Pedant helpers (role_util, node_util) to normalize option hashes.
unless Hash.method_defined?(:stringify_keys)
  class Hash
    # Returns a new Hash with all keys converted to Strings via to_s.
    # Does not deep-transform; Pedant only relied on shallow behavior previously.
    def stringify_keys
      each_with_object({}) do |(k, v), h|
        h[k.to_s] = v
      end
    end
  end
end
