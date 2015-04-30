Chef::Log.level= :fatal
start = Time.now
i = 0
query = "*:*"
puts "query: #{query}"
puts "running for 30 sec"
while (Time.now - start) < 30 do
  nodes = search(:node, query)
  i += 1
  puts "#{i}: #{nodes.length}"
end
puts "Time: #{(Time.now - start)}"
