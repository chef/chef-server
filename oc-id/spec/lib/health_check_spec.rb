require 'spec_helper'

class ErchefUnauthorizedException < StandardError
  def self.exception
    Net::HTTPServerException.exception('401 Unauthorized', Net::HTTPServerError)
  end
end

class ErchefOtherException < StandardError
  def self.exception
    Net::HTTPServerException.exception('Other', Net::HTTPServerError)
  end
end

describe HealthCheck do
  context 'erchef is having problems' do
    before do
      allow(subject).to receive(:chef).and_raise(Errno::ETIMEDOUT)
      allow(ActiveRecord::Base).to receive_message_chain('connection.query') { ['2'] }
    end

    it "should show 'not ok' for the top level status" do
      subject.check
      expect(subject.status).to eql('not ok')
    end

    it "should show something meaningful in the erchef status when there is an authentication problem" do
      chefstub = double('chef')
      allow(chefstub).to receive(:get_rest).with('_status').and_return({'status' => 'pong'})
      allow(chefstub).to receive(:get_rest).with('/users/pivotal').and_raise(ErchefUnauthorizedException)
      allow(subject).to receive(:chef).and_return(chefstub)
      subject.check
      expect(subject.erchef[:status]).to eql('authentication error')
    end

    it "should show something meaningful in the erchef status when there is a timeout" do
      subject.check
      expect(subject.erchef[:status]).to eql('timeout')
    end

    it "should show something meaningful in the erchef status when there is a different kind of error" do
      allow(subject).to receive(:chef).and_raise(ErchefOtherException)
      subject.check
      expect(subject.erchef[:status]).to eql('erroring')
    end

    it "should show that erchef is erroring if the status received is not 'pong'" do
      chefstub = double('chef')
      allow(chefstub).to receive(:get_rest).with('_status').and_return({'status' => 'pang'})
      allow(chefstub).to receive(:get_rest).with('/users/pivotal').and_return(true)
      allow(subject).to receive(:chef).and_return(chefstub)
      subject.check
      expect(subject.erchef[:status]).to eql('erroring')
    end
  end

  context 'postgres is having problems' do
    before do
      allow(subject).to receive(:chef).and_return(double('chef', :get_rest => {'status' => 'pong'}))
      allow(ActiveRecord::Base).to receive(:connection).and_raise(ActiveRecord::ConnectionTimeoutError)
    end

    it "should show 'not ok' for the top level status" do
      subject.check
      expect(subject.status).to eql('not ok')
    end

    it "should show something meaningful in the postgres status when there is a timeout" do
      subject.check
      expect(subject.postgres[:status]).to eql('timeout')
    end

    it "should show something meaningful in the postgres status when the connection is bad" do
      allow(ActiveRecord::Base).to receive(:connection).and_raise(PG::ConnectionBad)
      subject.check
      expect(subject.postgres[:status]).to eql('unreachable')
    end
  end

  context 'everything is running smoothly' do
    before do
      allow(subject).to receive(:chef).and_return(double('chef', :get_rest => {'status' => 'pong'}))
      allow(ActiveRecord::Base).to receive_message_chain('connection.query') { ['2'] }
      subject.check
    end

    it 'should show ok for the top level status' do
      expect(subject.status).to eql('ok')
    end

    it 'should show reachable for the erchef status' do
      expect(subject.erchef[:status]).to eql('reachable')
    end

    it 'should show reachable for the postgres status' do
      expect(subject.postgres[:status]).to eql('reachable')
    end

    it 'should show the number of postgres connections' do
      expect(subject.postgres[:connections]).to eql(2)
    end
  end
end
