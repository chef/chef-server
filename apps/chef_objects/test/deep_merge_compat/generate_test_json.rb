
require 'rubygems'
require 'yajl'

@test_index = 0

def data_path(basename)
  File.expand_path("../#{basename}.json", __FILE__)
end

def write_file(prefix, relname, content)
  File.open(data_path("#{prefix}_#{relname}"), "w") {|f| f.print(content)}
end

def print_example(mergee, other, expected)
  puts "# Example #@test_index"
  @test_index += 1

  prefix = @test_index.to_s.rjust(3, '0')

  write_file(prefix, :mergee,  Yajl::Encoder.encode(mergee) )
  write_file(prefix, :other,    Yajl::Encoder.encode(other) )
  write_file(prefix, :expected, Yajl::Encoder.encode(expected) )
end

################################################################################
# Examples taken from deep_merge's test suite.
# * In Erlang, we have to sort Lists before merging, so the expected results
# have all arrays sorted
# * In Chef, the `preserve_unmergeables` option is not used, so tests covering
# this feature are removed
#
# In deep_merge's terminology 'destination' is the one that gets merged on to,
# and 'source' is the object whose nodes "win" when there is a conflict.
################################################################################

hash_src = {'id' => '2'}
hash_dst = {}
print_example(hash_dst, hash_src, hash_src)

# test merging an hash w/array into blank hash
hash_src = {'region' => {'id' => ['227', '2']}}
hash_dst = {}
print_example(hash_dst, hash_src, hash_src)

# merge from empty hash
hash_src = {}
hash_dst = {"property" => ["2","4"]}
print_example(hash_dst, hash_src, hash_dst)

# merge to empty hash
hash_src = {"property" => ["2","4"]}
hash_dst = {}
print_example(hash_dst, hash_src, hash_src)

# simple string overwrite
hash_src = {"name" => "value"}
hash_dst = {"name" => "value1"}
print_example(hash_dst, hash_src, hash_src)

# simple string overwrite of empty hash
hash_src = {"name" => "value"}
hash_dst = {}
print_example(hash_dst, hash_src, hash_src)

################################################################################
# Erlang wont merge unsorted lists. Luckily, we can get away with sorting the
# results since order of array items is not important for indexing.
################################################################################
#
# hashes holding array
#hash_src = {"property" => ["1","3"]}
#hash_dst = {"property" => ["2","4"]}
#print_example(hash_dst, hash_src, {"property" => ["2","4","1","3"]})

# hashes holding array (sorted)
hash_src = {"property" => ["1","3"]}
hash_dst = {"property" => ["2","4"]}
print_example(hash_dst, hash_src, {'property' => ["1","2","3","4"]})

# hashes holding hashes holding arrays (array with duplicate elements is merged with dest then src
hash_src = {"property" => {"bedroom_count" => ["1", "2"], "bathroom_count" => ["1", "4+"]}}
hash_dst = {"property" => {"bedroom_count" => ["3", "2"], "bathroom_count" => ["2"]}}
print_example(hash_dst, hash_src, {"property" => {"bedroom_count" => %w{1 2 3}, "bathroom_count" => %w{1 2 4+}}})
#assert_equal({"property" => {"bedroom_count" => ["3","2","1"], "bathroom_count" => ["2", "1", "4+"]}}, hash_dst)

# hash holding hash holding array v string (string is overwritten by array)
hash_src = {"property" => {"bedroom_count" => ["1", "2"], "bathroom_count" => ["1", "4+"]}}
hash_dst = {"property" => {"bedroom_count" => "3", "bathroom_count" => ["2"]}}
print_example(hash_dst, hash_src, {"property" => {"bedroom_count" => ["1", "2"], "bathroom_count" => ["1","2","4+"]}})
#DeepMerge::deep_merge!(hash_src, hash_dst)
#assert_equal({"property" => {"bedroom_count" => ["1", "2"], "bathroom_count" => ["2","1","4+"]}}, hash_dst)

# hash holding hash holding string v array (array is overwritten by string)
hash_src = {"property" => {"bedroom_count" => "3", "bathroom_count" => ["1", "4+"]}}
hash_dst = {"property" => {"bedroom_count" => ["1", "2"], "bathroom_count" => ["2"]}}
print_example(hash_dst, hash_src, {"property" => {"bedroom_count" => "3", "bathroom_count" => ["1","2","4+"]}})
#DeepMerge::deep_merge!(hash_src, hash_dst)
#assert_equal({"property" => {"bedroom_count" => "3", "bathroom_count" => ["2","1","4+"]}}, hash_dst)

# hash holding hash holding hash v array (array is overwritten by hash)
hash_src = {"property" => {"bedroom_count" => {"king_bed" => 3, "queen_bed" => 1}, "bathroom_count" => ["1", "4+"]}}
hash_dst = {"property" => {"bedroom_count" => ["1", "2"], "bathroom_count" => ["2"]}}
print_example(hash_dst, hash_src, {"property" => {"bedroom_count" => {"king_bed" => 3, "queen_bed" => 1}, "bathroom_count" => ["1","2","4+"]}})
#DeepMerge::deep_merge!(hash_src, hash_dst)
#assert_equal({"property" => {"bedroom_count" => {"king_bed" => 3, "queen_bed" => 1}, "bathroom_count" => ["2","1","4+"]}}, hash_dst)

# 3 hash layers holding integers (integers are overwritten by source)
hash_src = {"property" => {"bedroom_count" => {"king_bed" => 3, "queen_bed" => 1}, "bathroom_count" => ["1", "4+"]}}
hash_dst = {"property" => {"bedroom_count" => {"king_bed" => 2, "queen_bed" => 4}, "bathroom_count" => ["2"]}}
print_example(hash_dst, hash_src, {"property" => {"bedroom_count" => {"king_bed" => 3, "queen_bed" => 1}, "bathroom_count" => ["1","2","4+"]}})
#DeepMerge::deep_merge!(hash_src, hash_dst)
#assert_equal({"property" => {"bedroom_count" => {"king_bed" => 3, "queen_bed" => 1}, "bathroom_count" => ["2","1","4+"]}}, hash_dst)

# 3 hash layers holding arrays of int (arrays are merged)
hash_src = {"property" => {"bedroom_count" => {"king_bed" => [3], "queen_bed" => [1]}, "bathroom_count" => ["1", "4+"]}}
hash_dst = {"property" => {"bedroom_count" => {"king_bed" => [2], "queen_bed" => [4]}, "bathroom_count" => ["2"]}}
print_example(hash_dst, hash_src, {"property" => {"bedroom_count" => {"king_bed" => [2,3], "queen_bed" => [1,4]}, "bathroom_count" => ["1","2","4+"]}})
#DeepMerge::deep_merge!(hash_src, hash_dst)
#assert_equal({"property" => {"bedroom_count" => {"king_bed" => [2,3], "queen_bed" => [4,1]}, "bathroom_count" => ["2","1","4+"]}}, hash_dst)

# 1 hash overwriting 3 hash layers holding arrays of int
hash_src = {"property" => "1"}
hash_dst = {"property" => {"bedroom_count" => {"king_bed" => [2], "queen_bed" => [4]}, "bathroom_count" => ["2"]}}
print_example(hash_dst, hash_src, {"property" => "1"})
#DeepMerge::deep_merge!(hash_src, hash_dst)
#assert_equal({"property" => "1"}, hash_dst)

# 3 hash layers holding arrays of int (arrays are merged) but second hash's array is overwritten
hash_src = {"property" => {"bedroom_count" => {"king_bed" => [3], "queen_bed" => [1]}, "bathroom_count" => "1"}}
hash_dst = {"property" => {"bedroom_count" => {"king_bed" => [2], "queen_bed" => [4]}, "bathroom_count" => ["2"]}}
print_example(hash_dst, hash_src, {"property" => {"bedroom_count" => {"king_bed" => [2,3], "queen_bed" => [1,4]}, "bathroom_count" => "1"}})
#DeepMerge::deep_merge!(hash_src, hash_dst)
#assert_equal({"property" => {"bedroom_count" => {"king_bed" => [2,3], "queen_bed" => [4,1]}, "bathroom_count" => "1"}}, hash_dst)

# 3 hash layers holding arrays of int, but one holds int. This one overwrites, but the rest merge
hash_src = {"property" => {"bedroom_count" => {"king_bed" => 3, "queen_bed" => [1]}, "bathroom_count" => ["1"]}}
hash_dst = {"property" => {"bedroom_count" => {"king_bed" => [2], "queen_bed" => [4]}, "bathroom_count" => ["2"]}}
print_example(hash_dst, hash_src, {"property" => {"bedroom_count" => {"king_bed" => 3, "queen_bed" => [4,1]}, "bathroom_count" => ["2","1"]}})
#DeepMerge::deep_merge!(hash_src, hash_dst)
#assert_equal({"property" => {"bedroom_count" => {"king_bed" => 3, "queen_bed" => [4,1]}, "bathroom_count" => ["2","1"]}}, hash_dst)

# 3 hash layers holding arrays of int, but source is incomplete.
hash_src = {"property" => {"bedroom_count" => {"king_bed" => [3]}, "bathroom_count" => ["1"]}}
hash_dst = {"property" => {"bedroom_count" => {"king_bed" => [2], "queen_bed" => [4]}, "bathroom_count" => ["2"]}}
print_example(hash_dst, hash_src, {"property" => {"bedroom_count" => {"king_bed" => [2,3], "queen_bed" => [4]}, "bathroom_count" => ["2","1"]}})
#DeepMerge::deep_merge!(hash_src, hash_dst)
#assert_equal({"property" => {"bedroom_count" => {"king_bed" => [2,3], "queen_bed" => [4]}, "bathroom_count" => ["2","1"]}}, hash_dst)

# 3 hash layers holding arrays of int, but source is shorter and has new 2nd level ints.
hash_src = {"property" => {"bedroom_count" => {2=>3, "king_bed" => [3]}, "bathroom_count" => ["1"]}}
hash_dst = {"property" => {"bedroom_count" => {"king_bed" => [2], "queen_bed" => [4]}, "bathroom_count" => ["2"]}}
print_example(hash_dst, hash_src, {"property" => {"bedroom_count" => {2=>3, "king_bed" => [2,3], "queen_bed" => [4]}, "bathroom_count" => ["2","1"]}})
#DeepMerge::deep_merge!(hash_src, hash_dst)
#assert_equal({"property" => {"bedroom_count" => {2=>3, "king_bed" => [2,3], "queen_bed" => [4]}, "bathroom_count" => ["2","1"]}}, hash_dst)

# 3 hash layers holding arrays of int, but source is empty
hash_src = {}
hash_dst = {"property" => {"bedroom_count" => {"king_bed" => [2], "queen_bed" => [4]}, "bathroom_count" => ["2"]}}
print_example(hash_dst, hash_src, {"property" => {"bedroom_count" => {"king_bed" => [2], "queen_bed" => [4]}, "bathroom_count" => ["2"]}})
#DeepMerge::deep_merge!(hash_src, hash_dst)
#assert_equal({"property" => {"bedroom_count" => {"king_bed" => [2], "queen_bed" => [4]}, "bathroom_count" => ["2"]}}, hash_dst)

# 3 hash layers holding arrays of int, but dest is empty
hash_src = {"property" => {"bedroom_count" => {2=>3, "king_bed" => [3]}, "bathroom_count" => ["1"]}}
hash_dst = {}
print_example(hash_dst, hash_src, {"property" => {"bedroom_count" => {2=>3, "king_bed" => [3]}, "bathroom_count" => ["1"]}})
#DeepMerge::deep_merge!(hash_src, hash_dst)
#assert_equal({"property" => {"bedroom_count" => {2=>3, "king_bed" => [3]}, "bathroom_count" => ["1"]}}, hash_dst)

# This example is kinda silly, since JSON requires keys to be strings. Both
# Ruby JSON libraries convert the keys to strings, like so:
# "{\"[\\\"1\\\", \\\"2\\\", \\\"3\\\"]\":[\"1\",\"2\"],\"[\\\"4\\\", \\\"5\\\"]\":[\"3\"]}"
#
# hash holding arrays of arrays
# SKIP. see above.
#hash_src = {["1", "2", "3"] => ["1", "2"]}
#hash_dst = {["4", "5"] => ["3"]}
#print_example(hash_dst, hash_src, {["1","2","3"] => ["1", "2"], ["4", "5"] => ["3"]})
#DeepMerge::deep_merge!(hash_src, hash_dst)
#assert_equal({["1","2","3"] => ["1", "2"], ["4", "5"] => ["3"]}, hash_dst)

