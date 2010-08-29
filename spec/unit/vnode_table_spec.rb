require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

require 'opscode/expander/vnode_table'

describe Expander::VNodeTable do
  before do
    @vnode_table = Expander::VNodeTable.new
  end

  describe "when first created" do
    it "has no nodes" do
      @vnode_table.nodes.should be_empty
    end

    it "does not know what the least loaded node is" do
      @vnode_table.least_loaded_node.should be_nil
    end
  end

  describe "when one node's vnode info has been added" do
    before do
      @guid = "93226974-6d0b-4ca6-8d42-124dd55e0076"
      @hostname_f = "fermi.localhost"
      @pid = 12345
      @vnodes = (0..511).to_a
      @update = {:guid => @guid, :hostname_f => @hostname_f, :pid => @pid, :vnodes => @vnodes, :action => 'update'}
      @vnode_table.update_table(Yajl::Encoder.encode(@update))
    end

    it "has one vnode" do
      @vnode_table.should have(1).nodes
      @vnode_table.nodes.first.should == Expander::Node.from_hash(@update)
    end

    it "determines the only node is the least loaded node" do
      @vnode_table.least_loaded_node.should == Expander::Node.from_hash(@update)
    end

    it "removes the node from the table when it exits the cluster" do
      update = @update
      update[:action] = 'remove'
      @vnode_table.update_table(Yajl::Encoder.encode(update))
      @vnode_table.should have(0).nodes
    end

  end

  describe "when several nodes are in the table" do
    before do
      @node_1 = Expander::Node.new("93226974-6d0b-4ca6-8d42-124dd55e0076", "fermi.local", 12345)
      @node_1_hash = @node_1.to_hash
      @node_1_hash[:vnodes] = (0..511).to_a
      @node_1_hash[:action] = "update"
      @node_2 = Expander::Node.new("ad265988-f650-4a31-a97b-5dbf4db8e1b0", "fermi.local", 23425)
      @node_2_hash = @node_2.to_hash
      @node_2_hash[:vnodes] = (512..767).to_a
      @node_2_hash[:action] = "update"
      @vnode_table.update_table(Yajl::Encoder.encode(@node_1_hash))
      @vnode_table.update_table(Yajl::Encoder.encode(@node_2_hash))
    end

    it "determines the node with the least vnodes is the least loaded node" do
      @vnode_table.least_loaded_node.should == Expander::Node.from_hash(@node_2_hash)
    end
  end
end