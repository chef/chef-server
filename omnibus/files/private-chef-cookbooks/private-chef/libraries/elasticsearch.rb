# A class that knows about Elasticsearch configuration and usage.
class Elasticsearch

 KB = 1024
 MB = KB * KB
 GB = MB * KB

 def self.node_memory_in_units(node, which, unit)
   node[:memory][:total] =~ /^(\d+)kB/
   mem_in_kb = $1.to_i
   case unit
   when :kb, :kilobytes
     mem_in_kb
   when :bytes, :b
     mem_in_kb * 1024
   when :mb, :megabytes
     mem_in_kb / KB
   when :gb, :gigabytes
     mem_in_kb / (KB * KB)
   end
 end

  # Supplies the default total heap size for elasticsearch calculated as
  # the smaller of 1GB or 25% of total system memory.
  def self.heap_size_default(node)
    memory = node_memory_in_units(node, :total, :mb)
    [memory / 4, 1024].min
  end

  # Defaults to the larger of 1/16th of heap_size,
  # or 32MB.
  def self.new_size_default(node)
    [node['private_chef']['elasticsearch']['heap_size'] / 16, 32].max
  end
end
