require 'spec_helper'
require 'partybus/dsl_runner'

describe DSLRunner do
  subject { DSLRunner.new }

  describe '#define_migration' do
    context "with options:" do

      context "default" do
        subject do
          s = DSLRunner.new
          s.define_migration
          s
        end

        its(:api_version) { should == :v1 }
      end

      context ":api_version" do
        subject do
          s = DSLRunner.new
          s.define_migration :api_version => :custom_version
          s
        end

        its(:api_version) { should == :custom_version }
      end
    end
  end
end
