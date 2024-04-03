module FeatureFlagHelper
  def onetrust_enabled?
    Settings.enable_onetrust == true
  end
end
