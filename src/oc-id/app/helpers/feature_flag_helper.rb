module FeatureFlagHelper
  def enable_gtm_and_onetrust?
    ENV["ENABLE_GTM_AND_ONETRUST"] == "true"
  end
end
