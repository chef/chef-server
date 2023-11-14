module FeatureFlagHelper
  def gtm_enabled?
    ENV["ENABLE_GTM"] == "true"
  end

  def onetrust_enabled?
    ENV["ENABLE_ONETRUST"] == "true"
  end
end
