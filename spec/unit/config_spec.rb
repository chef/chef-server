require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

require 'stringio'
require 'opscode/expander/config'

describe Expander::Config do
  before do
    @config = Expander::Config.instance
    @config.reset!
    @config.index = 1
    @config.node_count = 5
  end

  it "stores the number of nodes" do
    @config.node_count.should == 5
  end

  it "stores the position in the node ring" do
    @config.index.should == 1
  end

  it "computes the vnodes the node should claim" do
    @config.vnode_numbers.should == (0..203).to_a
  end

  it "assigns any remainder to the last node in the ring" do
    @config.index = 5
    @config.vnode_numbers.should == (816..1023).to_a
  end

  it "raises an invalid config error when then node index is not set" do
    @config.index = nil
    lambda { @config.validate! }.should raise_error(Expander::Config::InvalidConfiguration)
  end

  it "raises an invalid config error when the node count is not set" do
    @config.node_count = nil
    lambda { @config.validate! }.should raise_error(Expander::Config::InvalidConfiguration)
  end

  it "raises an invalid config error when the index is greater than the node count" do
    @config.node_count = 5
    @config.index = 10
    lambda { @config.validate! }.should raise_error(Expander::Config::InvalidConfiguration)
  end

  it "exits when the config is invalid" do
    stdout = StringIO.new
    @config.reset!(stdout)
    @config.node_count = nil
    lambda {@config.fail_if_invalid}.should raise_error(SystemExit)
    stdout.string.should match(/You need to specify the total number of nodes in the cluster/)
  end

end