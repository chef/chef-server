require 'spec_helper'

describe 'OAuth' do

  subject { page }
  before { Settings.stub_chain(:doorkeeper, :administrators).and_return(['rainbowdash']) }

  describe 'applications list' do

    before do
      visit signin_path
      fill_in 'Username', :with => user.username
      fill_in 'Password', :with => user.password
      click_button 'Sign In'
    end

    describe 'as an non-administrator' do
      let(:user) { FactoryGirl.build(:user) }

      before { visit oauth_applications_path }
      
      it { should_not have_content 'Your Applications' }
      it { should_not have_link('New Application') }
      it { should have_content('Your Authorized Applications') }
    end

    describe 'as an administrator' do
      let(:user) { FactoryGirl.build(:administrator) }
      
      before { visit oauth_applications_path }

      it { should have_content 'Your Applications' }
      it { should have_link('New Application') }
      it { should_not have_content('Your Authorized Applications') }
    end

  end

end
