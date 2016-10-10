#!/usr/bin/env ruby
class SpaceAnalyzer
  def initialize(max_percent=3, base_dir=".")
    @max_percent = max_percent
    @base_dir = base_dir
    @path_data = {}
  end

  def analyze!
    collect
    report
  end

  private

  def collect
    @total_size = size_of(@base_dir)
    @path_data[@base_dir] = {
      total: @total_size,
      perc: 100,
      subdirs: collect_for(@base_dir)
    }
    @path_data
  end

  def collect_for(base_dir)
    ret = {}
    Dir.glob("#{base_dir}/*") do |i|
      size = size_of(i)
      perc = (Float(size)/Float(@total_size)) * 100
      next if perc < @max_percent
      ret[i] = {
        total: size,
        perc: perc,
        subdirs: collect_for(i)
      }
    end
    ret
  end

  def report(paths=sort_by_size(@path_data), level=0)
    paths.each do |path, data|
      puts sprintf("#{'  ' * level }#{data[:total]}MB (%.2f%%) #{path}", data[:perc])
      report(sort_by_size(data[:subdirs]), level+1)
    end
  end

  def sort_by_size(hash)
    hash.sort_by { |k, v| v[:total] }.reverse.inject({}) {|h,i| h[i.first] = i.last; h }
  end

  def size_of(dir)
    Integer(`du -sm #{dir}`.chomp.split(' ').first)
  end
end

p = if ARGV[0]
      ARGV[0].to_i
    else
      3
    end
s = SpaceAnalyzer.new(p)
s.analyze!
