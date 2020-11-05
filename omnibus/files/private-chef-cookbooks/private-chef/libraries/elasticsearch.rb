# frozen_string_literal: true

# A class that knows about Elasticsearch configuration and usage.
class Elasticsearch
  KB = 1024
  MB = KB * KB
  GB = MB * KB
  MIN_HEAP_SIZE = 1 * 1024 # in mb
  MAX_HEAP_SIZE = 26 * 1024 # in mb

  def self.node_memory_in_units(node, _which, unit)
    node[:memory][:total] =~ /^(\d+)kB/
    mem_in_kb = Regexp.last_match(1).to_i
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
  # 25% of the system memory bounded between 1GB - 26GB
  # https://www.elastic.co/guide/en/elasticsearch/reference/current/heap-size.html
  def self.heap_size_default(node)
    memory = node_memory_in_units(node, :total, :mb)
    value = memory / 4
    if value > MAX_HEAP_SIZE
      value = MAX_HEAP_SIZE
    elsif value < MIN_HEAP_SIZE
      value = MIN_HEAP_SIZE
    end
    value
  end

  # Defaults to the larger of 1/16th of heap_size,
  # or 32MB.
  def self.new_size_default(node)
    [node['private_chef']['elasticsearch']['heap_size'] / 16, 32].max
  end
end
