Chef::Log.level= :fatal
start = Time.now
for i in (1..100) do
  puts i
  n = Chef::Node.new
  data = JSON.parse(IO.read('/Users/johnkeiser/opscode/opscode-benchmark/data/ohai_data.json'))
  n.consume_attributes(data)
  n.name("newnode-#{i}")
  n.save
#  rest.post_rest("nodes", node)
end
