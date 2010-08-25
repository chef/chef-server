require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

require 'opscode/expander/vnode'
require 'fiber'

describe Expander::VNode do
  before do
    @vnode = Expander::VNode.new("2342", :supervise_interval => 0.1)
    @log_stream = StringIO.new
    @vnode.log.init(@log_stream)
  end

  it "has the vnode number it was created with" do
    @vnode.vnode_number.should == '2342'
  end

  it "has a queue named after its vnode number" do
    @vnode.queue_name.should == "vnode_2342"
  end

  it "has a binding key for its vnode number" do
    @vnode.binding_key.should == "*.vnode_2342"
  end

  describe "when connecting to rabbitmq" do
    it "disconnects if there is another subscriber" do
      f = Fiber.current
      b = Bunny.new(OPSCODE_EXPANDER_MQ_CONFIG)
      b.start
      b.exchange("opscode-platform", :type => :topic)
      q = b.queue(@vnode.queue_name)
      t = Thread.new { q.subscribe { |message| nil }}

      AMQP.start(OPSCODE_EXPANDER_MQ_CONFIG) do
        Fiber.new do
          MQ.topic('foo')
          EM.add_timer(0.5) do
            AMQP.stop
            EM.stop
            f.resume
          end
          @vnode.start
        end.resume
      end
      t.kill
      b.stop

      @vnode.should be_stopped
      @log_stream.string.should match(/Detected extra consumers/)
    end
  end

end