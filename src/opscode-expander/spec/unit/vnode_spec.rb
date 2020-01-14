require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

require 'opscode/expander/vnode_supervisor'
require 'opscode/expander/vnode'

describe Expander::VNode do
  before do
    @supervisor = Expander::VNodeSupervisor.new
    @vnode = Expander::VNode.new("2342", @supervisor, :supervise_interval => 0.1)
    @log_stream = StringIO.new
    @vnode.log.init(@log_stream)
  end

  it "has the vnode number it was created with" do
    @vnode.vnode_number.should == 2342
  end

  it "has a queue named after its vnode number" do
    @vnode.queue_name.should == "vnode-2342"
  end

  it "has a control queue name" do
    @vnode.control_queue_name.should == "vnode-2342-control"
  end

end
