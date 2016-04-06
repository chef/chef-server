require "veil/hasher"
require "veil/credential"
require "forwardable"

module Veil
  class CredentialCollection
    class Base
      class << self
        def create(hash = {})
          new(hash)
        end
      end

      extend Forwardable

      attr_reader :credentials, :hasher

      def_delegators :@credentials, :size, :length, :find, :map, :select, :each, :[]

      def initialize(opts = {})
        @hasher = Veil::Hasher.create(opts[:hasher] || {})
        @credentials = expand_credentials_hash(opts[:credentials] || {})
      end

      def to_hash
        {
          type: self.class.name,
          hasher: hasher.to_h,
          credentials: credentials_as_hash
        }
      end
      alias_method :to_h, :to_hash

      def save
        raise "Save has not been implemented for this class"
      end

      def rotate(set_or_cred, cred = nil)
        if cred && credentials.key?(set_or_cred) && credentials[set_or_cred].key?(cred)
          credentials[set_or_cred][cred].rotate(hasher)
        elsif credentials.key?(set_or_cred)
          if credentials[set_or_cred].is_a?(Hash)
            credentials[set_or_cred].each { |_s, c| c.rotate(hasher) }
          else
            credentials[set_or_cred].rotate(hasher)
          end
        end
      end

      # Add a new credential to the credentials
      #
      # @param [Hash] args
      #
      def add(*args)
        params = { name: nil, length: 128, value: nil }
        case args.length
        when 1
          # add('foo')
          params[:name] = args.first
        when 2
          if args.all? { |a| a.is_a?(String) }
            # add('my_app', 'foo')
            set, params[:name] = args
          elsif args[1].is_a?(Hash)
            # add('my_app', value: 'something')
            # add('foo', length: 50)
            params[:name] = args.first
            params.merge!(args[1])
          end
        when 3
          # add('my_app', 'foo', value: 'something')
          # add('my_app', 'foo', length: 50)
          set, params[:name] = args[0], args[1]
          params.merge!(args[2])
        else
          raise ArgumentError, "wrong number of arguments (given #{args.length}, expected 1-3)"
        end

        params[:name] = params[:name].to_s

        if params[:value]
          params[:length] = params[:value].length
        else
          params[:value] = hasher.encrypt([params[:name], 0].join)
        end

        new_cred = Veil::Credential.new(params)

        if set
          credentials[set] ||= {}

          return credentials[set][params[:name]] if credentials[set].key?(params[:name])

          credentials[set][params[:name]] = new_cred
        else
          return credentials[params[:name]] if credentials.key?(params[:name])

          credentials[params[:name]] = new_cred
        end
      end
      alias_method :<<, :add

      def remove(set_or_cred, cred = nil)
        if set_or_cred && cred && credentials.key?(set_or_cred)
          credentials[set_or_cred].delete(cred)
        else
          credentials.delete(set_or_cred)
        end
      end
      alias_method :delete, :remove

      def rotate_hasher
        @hasher = Veil::Hasher.create
        credentials.each do |cred_or_set_name, cred_or_set|
          if cred_or_set.is_a?(Veil::Credential)
            cred_or_set.rotate(hasher)
          else
            cred_or_set.each { |_set, cred| cred.rotate(hasher) }
          end
        end
      end

     private

      def expand_credentials_hash(creds_hash)
        expanded = Hash.new

        creds_hash.each do |cred_or_set_name, cred_or_set_attrs|
          if cred_or_set_attrs.key?(:type) && cred_or_set_attrs[:type] == "Veil::Credential"
            expanded[cred_or_set_name.to_s] = Veil::Credential.create(cred_or_set_attrs)
          else
            cred_or_set_attrs.each do |name, opts|
              expanded[cred_or_set_name.to_s] ||= Hash.new
              expanded[cred_or_set_name.to_s][name.to_s] = Veil::Credential.create(opts)
            end
          end
        end

        expanded
      end

      def credentials_as_hash
        hash = Hash.new

        credentials.each do |cred_or_set_name, cred_or_set_attrs|
          if cred_or_set_attrs.is_a?(Hash)
            cred_or_set_attrs.each do |name, cred|
              hash[cred_or_set_name] ||= Hash.new
              hash[cred_or_set_name][name] = cred.to_hash
            end
          else
            hash[cred_or_set_name] = cred_or_set_attrs.to_hash
          end
        end

        hash
      end
    end
  end
end
