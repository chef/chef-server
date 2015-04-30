require 'spec_helper'

describe Partybus do

  subject { Partybus }

  it 'should be a module' do
    subject.class.should == Module
  end

end
