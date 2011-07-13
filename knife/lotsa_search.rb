Chef::Log.level= :fatal
start = Time.now
i = 0
query = (1..20).map { |x| "name:newnode-#{x}" }.join(" ")
puts query
while (Time.now - start) < 30 do
  nodes = search(:node, query)
  i += 1
  puts "#{i}: #{nodes.length}"
  # nodes.each { |n| puts n.name }
end
puts "Time: #{(Time.now - start)}"
