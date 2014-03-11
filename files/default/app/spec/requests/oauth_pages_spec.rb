require 'spec_helper'

describe 'OAuth' do
  subject { page }

  describe 'applications list' do
    before { visit oauth_applications_path }
    it { should have_content 'Your Applications' }
    it { should have_link('New Application') } 
  end
  
end
