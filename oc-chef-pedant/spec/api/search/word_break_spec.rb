require 'pedant/rspec/search_util'
require 'pedant/rspec/node_util'

describe 'Search API endpoint', :search do

  include Pedant::RSpec::SearchUtil
  include Pedant::RSpec::NodeUtil

  # TODO: until we rename requestors
  shared(:admin_requestor){admin_user}
  shared(:requestor){admin_requestor}

  context "word break handling" do
    let(:request_method){:GET}
    let(:request_url){api_url("/search/node")}
    SPECIAL_CHARS = '!"$%&()*+,-:;<=>?@[\]%_`{|}~\'\\'
    PERMITTED_QUERY_CHARS = '[]\\"!(){}^~*?:'

    # We're going to cycle a lot of tests here, so let's create just one item to meet all the data conditions
    # we want to validate.
    before :all do
      # confirm 201 status as a sanity check
      n = new_node("search_supernode").tap do |node|
        SPECIAL_CHARS.each_char do |c|
          node["default"]["attrtest#{SPECIAL_CHARS.index(c)}"] = "hello#{c}world"
          node["default"]["key#{c}abc"] = "dlrowolleh#{SPECIAL_CHARS.index(c)}"
        end
      end
      add_node(admin_requestor, n).should look_like({:status => 201 })
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

    SPECIAL_CHARS.each_char do |char|
      describe "when searching for an attribute value containing the special character '#{char}'" do
        c = PERMITTED_QUERY_CHARS.include?(char) ? "\\#{char}" : char
        attrname = "attrtest#{SPECIAL_CHARS.index(char)}"
        [ [ "an exact match on name and value", "#{attrname}:hello#{c}world", :uses_char, true],
          [ "a wildcard match on attribute name with exact value", "*:hello#{c}world", :uses_char, true ],
          [ "a wildcard in attribute value around the special char", "#{attrname}:*#{c}*", :uses_char, true],
          [ "a wildcard in attribute value before the special char with trailing match", "#{attrname}:*#{c}", :uses_char, false],
          [ "a wildcard in attribute value before the special char with no trailing match", "#{attrname}:*#{c}world", :uses_char, true],
          [ "a wildcard in attribute value after the special char with prefixing match", "#{attrname}:hello#{c}*", :uses_char, true],
          [ "a wildcard in attribute value after the special char with no prefixing match", "#{attrname}:#{c}*", :uses_char, false],
          [ "an exact search for a partial match of the first word", "#{attrname}:hello", :does_not_use_char, false],
          [ "an exact search for a partial match of the second word", "#{attrname}:world",:does_not_use_char, false],
          [ "a search for attribute value as a string without the special character", "#{attrname}:*\"hello world\"*",:does_not_use_char, false],
        ].each do | description, query, usage, find_expected|
          # We can only query on valid characters to use in a query - so
          # anything that uses the special char may not be valid. If it doesn't use it
          # let it through.
          if usage == :does_not_use_char or (usage == :uses_char and PERMITTED_QUERY_CHARS.include?(char))
            it "with #{description}, should #{find_expected ? "find" : "not find"} the node" do
              expected = find_expected ? wb_node_found_result : wb_node_not_found_result
              expect(search_result("node", query)).to look_like expected
            end
          end
        end
      end

      describe "when searching for attribute an attribute key containing the special character '#{char}'" do
        c = PERMITTED_QUERY_CHARS.include?(char) ? "\\#{char}" : char
        attrval = "dlrowolleh#{SPECIAL_CHARS.index(char)}"
        [ [ "an exact match on name and value", "key#{c}abc:#{attrval}", :uses_char, true],
          [ "a wildcard match on key name with exact value", "*:#{attrval}", :uses_char, true ],
          [ "a wildcard in attribute key around the special char", "*#{c}*:#{attrval}", :uses_char, true],
          [ "a wildcard in attribute key before the special char with trailing match", "*#{c}abc:#{attrval}", :uses_char, true],
          [ "a wildcard in attribute key before the special char with no trailing match", "*#{c}:#{attrval}", :uses_char, false],
          [ "a wildcard in attribute key after the special char with no prefixing match", "#{c}*:#{attrval}", :uses_char, false],
          [ "a wildcard in attribute key after the special char with prefixing match", "key#{c}*:#{attrval}", :uses_char, true],
          [ "an exact search for a partial match of the first word", "key:#{attrval}", :does_not_use_char, false],
          [ "an exact search for a partial match of the second word", "abc:#{attrval}",:does_not_use_char, false],
          [ "a search for attribute key as a string without the special character", "*\"key abc\"*:*",:does_not_use_char, false],
        ].each do | description, query, usage, find_expected|
          # We can only query on valid characters to use in a query - so
          # anything that uses the special char isn't valid. If it doesn't use it
          # let it through.
          if usage == :does_not_use_char or (usage == :uses_char and PERMITTED_QUERY_CHARS.include?(char))
            it "with #{description}, should #{find_expected ? "find" : "not find"} the node" do
              expected = find_expected ? wb_node_found_result : wb_node_not_found_result
              expect(search_result("node", query)).to look_like expected
            end
          end
        end
      end
    end
  end
end
