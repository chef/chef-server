require "spec_helper"

describe ApplicationHelper do
  let(:chef) { double('chef', :put_rest => {'private_key' => 'my awesome private key'}) }
  let(:user) { User.new }

  before do
    allow(user).to receive(:chef).and_return(chef)
    allow(User).to receive(:authenticate).and_return(true)
  end

  describe '#error_message_for' do
    it 'includes the attribute name in the message' do
      user.update_password(:password => 'haha')
      expect(helper.error_message_for(user, :new_password)).to eql('<small class="error">New Password must not be blank.</small>')
    end

    it 'works for base attributes too' do
      user.update_password(:new_password => 'haha', :password_confirmation => 'nope')
      expect(helper.error_message_for(user, :base)).to eql('<small class="error">The password you entered does not match the confirmation password.</small>')
    end
  end
end
