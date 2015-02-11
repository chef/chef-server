#!/opt/chef/embedded/bin/ruby

require 'rubygems'
require 'chef/json_compat'

class OhNo
  def ohai
    if !@ohai
      raw = `ohai -lerror`
      @ohai = Chef::JSONCompat.from_json(raw)
    end
    @ohai
  end

  def machine
    ohai["kernel"]["machine"]
  end

  def os
    ohai["platform"] + "-" + ohai["platform_version"]
  end

  def write(path)
    open(path, 'w') do |f|
      f.write("export machine=#{machine}\n")
      f.write("export os=#{os}\n")
    end
  end
end

OhNo.new.write("machine_info")
