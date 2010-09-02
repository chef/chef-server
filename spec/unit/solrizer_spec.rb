require File.expand_path(File.dirname(__FILE__) + '/../spec_helper')

require 'stringio'
require 'opscode/expander/solrizer'
require 'yajl'

describe Expander::Solrizer do

  describe "when created with an add request" do
    before do
      @indexer_payload = {:item => {:foo => {:bar => :baz}}, :type => :node, :database => :testdb, :id => "2342"}
      @update_object = {:action => "add", :payload => @indexer_payload}
      @update_json = Yajl::Encoder.encode(@update_object)
      @solrizer = Expander::Solrizer.new(@update_json)

      @log_stream = StringIO.new
      @solrizer.log.init(@log_stream)
    end

    it "extracts the indexing-specific payload from the update message" do
      @solrizer.indexer_payload.should == { 'item' => {'foo' => {'bar' => "baz"}},
                                            'type' => 'node',
                                            'database' => 'testdb', 'id' => "2342"}
    end

    it "extracts the action from the update message" do
      @solrizer.action.should == "add"
    end

    it "extracts the item to update from the update message" do
      @solrizer.chef_object.should == {"foo" => {"bar" => "baz"}}
    end

    it "extracts the database name from the update message" do
      @solrizer.database.should == "testdb"
    end

    it "extracts the object id from the update message" do
      @solrizer.obj_id.should == "2342"
    end

    it "extracts the object type from the update message" do
      @solrizer.obj_type.should == "node"
    end

    it "generates the flattened and expanded representation of the object" do
      @solrizer.flattened_object.should == {"foo"                    => ["bar"],
                                            "X_bar"                  => ["baz"],
                                            "foo_X"                  => ["baz"],
                                            "foo_bar"                => ["baz"],
                                            "bar"                    => ["baz"],
                                            "X_CHEF_id_CHEF_X"       =>["2342"],
                                            "X_CHEF_database_CHEF_X" =>["testdb"],
                                            "X_CHEF_type_CHEF_X"     =>["node"]}
    end

    it "converts the flattened and expanded object to compact readable XML" do
      expected=<<-POINTY_BRACKETS4LIFE
<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<add><doc><field name=\"foo\">bar</field><field name=\"X_bar\">baz</field><field name=\"foo_X\">baz</field><field name=\"foo_bar\">baz</field><field name=\"bar\">baz</field><field name=\"X_CHEF_id_CHEF_X\">2342</field><field name=\"X_CHEF_database_CHEF_X\">testdb</field><field name=\"X_CHEF_type_CHEF_X\">node</field></doc></add>
POINTY_BRACKETS4LIFE
      @solrizer.pointyize_add.should == expected
    end

  end

  describe "when created with a delete reques" do
    before do
      @indexer_payload = {:id => "2342"}
      @update_object = {:action => "add", :payload => @indexer_payload}
      @update_json = Yajl::Encoder.encode(@update_object)
      @solrizer = Expander::Solrizer.new(@update_json)
    end

    it "extracts the indexer payload" do
      @solrizer.indexer_payload.should == {"id" => "2342"}
    end

    it "extracts the object id" do
      @solrizer.obj_id.should == "2342"
    end

    it "converts the delete request to XML" do
      expected = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<delete><id>2342</id></delete>\n"
      @solrizer.pointyize_delete.should == expected
    end

  end

end