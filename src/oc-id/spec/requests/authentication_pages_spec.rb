require 'spec_helper'

describe 'Authentication' do
  subject { page }

  describe 'sign-in' do
    before { visit signin_path }

    it { should have_content 'Sign In' }
    it { should have_field 'username' }
    it { should have_field 'password' }
    it { should have_button 'Sign In' }
    it { should have_link 'Forgot your password?' }
    it { should have_link 'Sign Up' }

    describe 'success' do
      let(:user) { FactoryGirl.build(:user) }

      before do
        fill_in 'username', :with => user.username
        fill_in 'password', :with => user.password
        click_button 'Sign In'
      end

      it { should have_link 'Sign Out', :href => signout_path }
      it { should_not have_link 'Sign In' }
      it { should_not have_link 'Sign Up' }
      it { should have_content 'Applications' }

      describe 'and then sign out' do
        before { click_link 'Sign Out' }
        it { should have_button 'Sign In' }
        it { should have_link 'Sign Up' }
        it { should have_link 'Forgot your password?' }
      end
    end

    describe 'success with email' do
      let(:user) { FactoryGirl.build(:user) }

      before do
        fill_in 'username', :with => user.email
        fill_in 'password', :with => user.password
        click_button 'Sign In'
      end

      it { should have_link 'Sign Out', :href => signout_path }
      it { should_not have_link 'Sign In' }
      it { should_not have_link 'Sign Up' }
      it { should have_content 'Applications' }

      describe 'and then sign out' do
        before { click_link 'Sign Out' }
        it { should have_button 'Sign In' }
        it { should have_link 'Sign Up' }
        it { should have_link 'Forgot your password?' }
      end
    end

    describe 'failure' do
      let(:user) { FactoryGirl.build(:user) }

      before do
        fill_in 'username', :with => user.username
        fill_in 'password', :with => 'something-totally-wrong'
        click_button 'Sign In'
      end

      it { should have_link 'Forgot your password?' }
      it { should have_link 'Sign Up' }
      it { should_not have_link 'Sign Out', :href => signout_path }
      it { should have_button 'Sign In' }
      it { should have_selector('.alert-box.alert') }

      describe 'and then visiting another page' do
        before { find(:css, '.logo').click }
        it { should_not have_selector('.alert-box.alert') }
      end
    end
  end
end
