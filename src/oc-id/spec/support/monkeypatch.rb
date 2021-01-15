# This patch is only required when using rails 4.2 with ruby 2.6 or greater
#
# https://github.com/rails/rails/issues/34790#issuecomment-681034561

# a workaround to avoid MonitorMixin double-initialize error
# https://github.com/rails/rails/issues/34790#issuecomment-681034561
if Gem::Version.new(RUBY_VERSION) >= Gem::Version.new('2.6.0')
  if Gem::Version.new(Rails.version) < Gem::Version.new('5.0.0')
    class ActionController::TestResponse < ActionDispatch::TestResponse
      def recycle!
        if Gem::Version.new(RUBY_VERSION) >= Gem::Version.new('2.7.0')
          @mon_data = nil
          @mon_data_owner_object_id = nil
        else
          @mon_mutex = nil
          @mon_mutex_owner_object_id = nil
        end
         initialize
      end
    end
  else
    $stderr.puts "Monkeypatch for ActionController::TestResponse is no longer needed"
  end
end
