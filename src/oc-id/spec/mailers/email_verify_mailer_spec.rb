require 'spec_helper'

describe EmailVerifyMailer, :type => :mailer do
  describe '#email_verify' do
    let(:username) { 'jimmy' }
    let(:email) { 'jim.kirk@federation-captains.org' }
    let(:user) do
      User.new({ username: username, email: email })
    end

    let(:email) { 'user@something.com' }

    let(:expiration) { 1.day.from_now.to_i }
    let(:signature) { Signature.new(user.username, user.email, expiration, Settings.secret_key_base, email) }
    subject(:mail) { EmailVerifyMailer.email_verify(user, email) }

    before do
      Timecop.freeze(Time.zone.parse('1970-01-01'))
      allow(Settings).to receive(:email_from_address).and_return('Test <test-from@example.com>')
    end

    after do
      Timecop.return
    end

    it 'sets the from address' do
      expect(mail.from).to eq ['test-from@example.com']
    end

    it 'sends to the user email' do
      expect(mail.to).to eq ['user@something.com']
    end

    it 'sets the subject' do
      expect(mail.subject).to eq 'Verify Your Chef Email'
    end

    it 'includes the link' do
      expect(mail.body.encoded).to include("http://example.com/id/profile/email?expires=86400&signature=#{signature}&user=#{CGI.escape(Base64.urlsafe_encode64(JSON.generate({username: user.username, email: email})))}")
    end
  end
end