require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

require 'opscode/expander/vnode_supervisor'

describe Expander::VNodeSupervisor do
  before do
    @log_stream = StringIO.new
    @local_node = Expander::Node.new("1101d02d-1547-45ab-b2f6-f0153d0abb34", "fermi.local", 12342)
    @vnode_supervisor = Expander::VNodeSupervisor.new
    @vnode_supervisor.instance_variable_set(:@local_node, @local_node)
    @vnode_supervisor.log.init(@log_stream)
    @vnode = Expander::VNode.new("42", @vnode_supervisor)
  end

  after do
    AMQP.start(OPSCODE_EXPANDER_MQ_CONFIG) do
      @vnode.stop
      EM.add_timer(0.01) { AMQP.stop;EM.stop }
    end
  end

  it "names its control queue after the FQDN + pid + guid" do
    @vnode_supervisor.control_queue_name.should == "fermi.local--12342--1101d02d-1547-45ab-b2f6-f0153d0abb34--control"
  end

  it "names its gossip queue after the FQDN + pid + guid" do
    @vnode_supervisor.gossip_queue_name.should == "fermi.local--12342--1101d02d-1547-45ab-b2f6-f0153d0abb34--gossip"
  end

  it "keeps a list of vnodes" do
    @vnode_supervisor.vnodes.should be_empty
    @vnode_supervisor.vnode_added(@vnode)
    @vnode_supervisor.vnodes.should == [42]
  end

  it "has a callback for vnode removal" do
    @vnode_supervisor.vnode_added(@vnode)
    @vnode_supervisor.vnodes.should == [42]
    @vnode_supervisor.vnode_removed(@vnode)
    @vnode_supervisor.vnodes.should be_empty
  end

  it "spawns a vnode" do
    AMQP.start(OPSCODE_EXPANDER_MQ_CONFIG) do
      @vnode_supervisor.spawn_vnode(42)
      MQ.topic('foo')
      EM.add_timer(0.1) do
        AMQP.stop
        EM.stop
      end
    end
    @vnode_supervisor.vnodes.should == [42]
  end

  it "subscribes to the control queue" do
    control_queue_msg = nil
    AMQP.start(OPSCODE_EXPANDER_MQ_CONFIG) do
      @vnode_supervisor.start_control_listener
      x = MQ.topic('opscode-platform-control')
      x.publish("hello_robot_overlord")
      EM.add_timer(0.1) do
        AMQP.stop
        EM.stop
      end
    end
  end

  it "declares its control queue and binds it to the exchange" do
    pending
  end

  it "periodically publishes its list of vnodes to the gossip queue" do
    pending
  end

  it "declares a node dead and delegates its vnodes when it receives a message on the control channel" do
    pending
  end

end