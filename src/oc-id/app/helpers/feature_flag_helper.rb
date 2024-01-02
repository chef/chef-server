module FeatureFlagHelper
  def gtm_enabled?
    Settings.enable_gtm == true
  end

  def onetrust_enabled?
    Settings.enable_onetrust == true
  end
end
