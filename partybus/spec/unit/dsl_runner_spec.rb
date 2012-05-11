require 'spec_helper'
require 'partybus/dsl_runner'

describe DSLRunner do
  subject { DSLRunner.new }

  describe '#define_upgrade' do
    context "with options:" do

      context "default" do
        subject do
          s = DSLRunner.new
          s.define_upgrade
          s
        end

        its(:api_version) { should == :v1 }
      end

      context ":api_version" do
        subject do
          s = DSLRunner.new
          s.define_upgrade :api_version => :custom_version
          s
        end

        its(:api_version) { should == :custom_version }
      end
    end
  end
end
