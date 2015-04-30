require "spec_helper"

describe ApplicationHelper do
  describe "sign_up_url" do
    it "delegates to settings" do
      allow(Settings).to receive(:sign_up_url).and_return("https://example.com")
      expect(helper.sign_up_url).to eq "https://example.com"
    end
  end
end
