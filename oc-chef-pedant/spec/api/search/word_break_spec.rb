require 'pedant/rspec/search_util'
require 'pedant/rspec/node_util'

describe 'Search API endpoint', :search do

  include Pedant::RSpec::SearchUtil
  include Pedant::RSpec::NodeUtil

  # TODO: until we rename requestors
  shared(:admin_requestor){admin_user}
  shared(:requestor){admin_requestor}

  context "word break handling", :focus do
    let(:request_method){:GET}
    let(:request_url){api_url("/search/node")}
    SPECIAL_CHARS = '!"$%&()*+,-:;<=>?@[\]%_`{|}~\'\\'
    QUERY_CHARS = '[]\\"!(){}^~*?:'

    # We're going to cycle a lot of tests here, so let's create just one item to meet all the data conditions
    # we want to validate.
    before :all do
      # confirm 201 status as a sanity check
      n = new_node("search_supernode").tap do |node|
        SPECIAL_CHARS.each_char do |c|
          node["default"]["attrtest#{SPECIAL_CHARS.index(c)}"] = "hello#{c}world"
          node["default"]["key#{SPECIAL_CHARS.index(c)}name"] = "dlrow#{c}olleh"
        end
      end
      add_node(admin_requestor, n).should look_like({:status => 201 })

      # Just force this once- all subsequent searches here will be against the same object.
      force_solr_commit
    end

    after :all do
      delete_node(admin_requestor, "search_supernode")
    end

    def wb_node_found_result
      { status: 200, body: { "rows" => [{ "name" => "search_supernode" }] } }
    end
    def wb_node_not_found_result
      { status: 200, body: { "rows" => [] } }
    end
    def wb_special_char(c)
    end

    SPECIAL_CHARS.each_char do |char|
      describe "when using attribute values containing the special character '#{char}'" do
          attrname = "attrtest#{SPECIAL_CHARS.index(char)}"
          c = if QUERY_CHARS.include? char
                "\\#{char}"
              else
               char
              end

          [ [ "an exact match on name and value", "#{attrname}:hello#{c}world", :uses_char, true],
            [ "a wildcard match on attribute name with exact value", "*:hello#{c}world", :uses_char, true ],
            [ "a wildcard in attribute value around the special char", "#{attrname}:*#{c}*", :uses_char, true],
            [ "a wildcard in attribute value before the special char", "#{attrname}:*#{c}", :uses_char, false],
            [ "a wildcard in attribute value after the special char", "#{attrname}:#{c}*", :uses_char, false],
            [ "an exact search for a partial match of the first word", "#{attrname}:hello", :does_not_use_char, false],
            [ "an exact search for a partial match of the second word", "#{attrname}:world",:does_not_use_char, false],
          ].each do | description, query, usage, find_expected|
            # We can only query on valid characters to use in a query - so
            # anything that uses the special char isn't valid. If it doesn't use it
            # let it through.
            if usage == :does_not_use_char or (usage == :uses_char and QUERY_CHARS.include?(char))
              it "and #{description}, should #{find_expected ? "find" : "not find"} the node" do
                expected = find_expected ? wb_node_found_result : wb_node_not_found_result
                expect(search_result("node", query)).to look_like expected
              end

            end
          end
        end
    end
  end
end
